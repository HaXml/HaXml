{-# LANGUAGE PatternGuards #-}
module Text.XML.HaXml.Schema.TypeConversion
  ( module Text.XML.HaXml.Schema.TypeConversion
  ) where

import Text.XML.HaXml.Types (QName(..),Name(..))
import Text.XML.HaXml.Namespaces (printableName)
import Text.XML.HaXml.Schema.XSDTypeModel     as XSD
import Text.XML.HaXml.Schema.HaskellTypeModel as Haskell
import Text.XML.HaXml.Schema.NameConversion
import Text.XML.HaXml.Schema.Parse (xsd)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')
import Data.Maybe (fromMaybe,fromJust)
import Data.Monoid

-- Some things we probably want to do.
-- * Build Maps from :
--       typename        to definition
--       element name    to definition
--       attribute name  to definition
--       (element) group to definition
--       attribute group to definition
-- * XSD types become top-level types in Haskell.
-- * XSD element decls also become top-level types in Haskell.
-- * Element groups get their own Haskell types too.
-- * Attributes and attribute groups do not become types, they are
--   simply constituent parts of an element.
-- * Resolve element/attribute references by inlining their names.

-- If a complextype definition includes nested in-line decls of other
-- types, we need to be able to lift them out to the top-level, then
-- refer to them by name only at the nested position.

data Environment =  Environment
    { env_type      :: Map QName (Either SimpleType ComplexType)
    , env_element   :: Map QName ElementDecl
    , env_attribute :: Map QName AttributeDecl
    , env_group     :: Map QName Group
    , env_attrgroup :: Map QName AttrGroup
    }

emptyEnv :: Environment
emptyEnv = Environment Map.empty Map.empty Map.empty Map.empty Map.empty

mkEnvironment :: Schema -> Environment
mkEnvironment s = foldl' item emptyEnv (schema_items s)
  where
    -- think about qualification, w.r.t targetNamespace, elementFormDefault, etc

    item env (Include _ _)       = env
    item env (Import _ _ _)      = env
    item env (Redefine _ _)      = env	-- revisit this
    item env (Annotation _)      = env
    item env (Simple st)         = simple env st
    item env (Complex ct)        = complex env ct
    item env (SchemaElement e)   = elementDecl env e
    item env (SchemaAttribute a) = attributeDecl env a
    item env (AttributeGroup g)  = attrGroup env g
    item env (SchemaGroup g)     = group env g

    simple env   (Primitive _)     = env
    simple env s@(Restricted n _ _ _)
                                   = env{env_type=Map.insert (N n) (Left s)
                                                             (env_type env)}
    simple env s@(ListOf n _ _ _)  = env{env_type=Map.insert (N n) (Left s)
                                                             (env_type env)}
    simple env s@(UnionOf n _ _ _) = env{env_type=Map.insert (N n) (Left s)
                                                             (env_type env)}

    -- Only toplevel names have global scope.
    -- Should we lift local names to toplevel with prefixed names?
    -- Or thread the environment explicitly through every tree-walker?
    -- Or resolve every reference to its referent in a single resolution pass?
    -- (Latter not good, because it potentially duplicates exprs?)
    complex env c
      | Nothing <- complex_name c = env
      | Just n  <- complex_name c = env{env_type=Map.insert (N n) (Right c)
                                                            (env_type env)}
    elementDecl env e
      | Right r <- elem_nameOrRef e = env
      | Left nt <- elem_nameOrRef e = env{env_element=Map.insert
                                                            (N $ theName nt) e
                                                            (env_element env)}
    attributeDecl env a
      | Right r <- attr_nameOrRef a = env
      | Left nt <- attr_nameOrRef a = env{env_attribute=
                                            Map.insert (N $ theName nt) a
                                                       (env_attribute env)}
    attrGroup env g
      | Right r <- attrgroup_nameOrRef g = env
      | Left n  <- attrgroup_nameOrRef g = env{env_attrgroup=Map.insert (N n) g
                                                           (env_attrgroup env)}
    group env g
      | Right r <- group_nameOrRef g = env
      | Left n  <- group_nameOrRef g = env{env_group=Map.insert (N n) g
                                                           (env_group env)}

convert :: Environment -> Schema -> [HighLevelDecl]
convert env s = concatMap item (schema_items s)
  where
    item (Include loc ann)    = [XSDInclude (xname loc) (comment ann)]
    item (Import uri loc ann) = [XSDImport  (xname loc) (comment ann)]
    item (Redefine _ _)       = [] -- ignoring redefinitions for now
    item (Annotation ann)     = [XSDComment (comment ann)]
    item (Simple st)          = simple st
    item (Complex ct)         = complex ct
    item (SchemaElement ed)   = topElementDecl ed
    item (SchemaAttribute ad) = [] -- attributeDecl ad
    item (AttributeGroup ag)  = [] -- attrgroup ag
    item (SchemaGroup g)      = group g

    simple (Primitive prim)     = []
    simple (Restricted n s r f) = [RestrictSimpleType (xname n) (nameOfSimple s)
                                                      Nothing]
    simple (ListOf n s r f)     = [NamedSimpleType    (xname n) (nameOfSimple s)
                                                      Nothing]
    simple (UnionOf n ss r f)   = error "Not yet implemented: unionOf"

    complex ct =
      let n = xname $ fromMaybe ("errorMissingName") (complex_name ct)
      in singleton $
      case complex_content ct of
        c@SimpleContent{}  ->
            case ci_stuff c of
                Left r  ->
                    RestrictSimpleType n ({-simple-}xname $ "Unimplemented")
                                     (comment (complex_annotation ct
                                              `mappend` ci_annotation c))
                Right e ->
                    ExtendSimpleType n
                                     ({-supertype-}XName $ extension_base e)
                                     ({-attrs-}snd $
                                      particleAttrs $ extension_newstuff e)
                                     (comment (complex_annotation ct
                                              `mappend` ci_annotation c
                                              `mappend` extension_annotation e))
        c@ComplexContent{} ->
            case ci_stuff c of
                Left r  ->
                    RestrictComplexType n ({-complex-}xname $ "Can'tBeRight")
                                     (comment (complex_annotation ct
                                              `mappend` ci_annotation c))
                Right e ->
                    let (es,as) = particleAttrs (extension_newstuff e) in
                    ExtendComplexType n
                                     ({-supertype-}XName $ extension_base e)
                                     ({-elems-}es)
                                     ({-attrs-}as)
                                     (comment (complex_annotation ct
                                              `mappend` ci_annotation c
                                              `mappend` extension_annotation e))
        c@ThisType{}       ->
            let (es,as) = particleAttrs (ci_thistype c) in
            ElementsAttrs n es as (comment (complex_annotation ct))

    topElementDecl :: XSD.ElementDecl -> [Haskell.HighLevelDecl]
    topElementDecl ed = case elem_nameOrRef ed of
        Left  n   -> singleton $
                     case theType n of
                       Nothing ->
                         error "Not implemented: contentInfo on topElementDecl"
                       --let (es,as) = contentInfo (elem_content ed) in
                       --ElementsAttrs ({-name-}xname $ theName n)
                       --              ({-elems-}es)
                       --              ({-attrs-}as)
                       --              (comment (elem_annotation ed))
                       Just t ->
                         ElementOfType Element{ elem_name = xname $ theName n
                                              , elem_type = XName t
                                              , elem_modifier = Single -- XXX
                                              , elem_comment =
                                                  (comment (elem_annotation ed))
                                              }
        Right ref -> case Map.lookup ref (env_element env) of
                       Nothing -> error $ "bad element reference "
                                          ++printableName ref
                       Just e' -> topElementDecl e'

    elementDecl :: XSD.ElementDecl -> Haskell.Element
    elementDecl ed = case elem_nameOrRef ed of
        Left  n   -> Element ({-name-}xname $ theName n)
                             ({-type-}XName $ fromJust $ theType n)
                             ({-modifier-}Haskell.Range $ elem_occurs ed)
                             (comment (elem_annotation ed))
        Right ref -> case Map.lookup ref (env_element env) of
                       Nothing -> error $ "bad element reference "
                                          ++printableName ref
                       Just e' -> elementDecl e'

    attributeDecl :: XSD.AttributeDecl -> [Haskell.Attribute]
    attributeDecl ad = case attr_nameOrRef ad of
        Left  n   -> singleton $
                     Attribute (xname $ theName n)
                               (maybe (maybe (error "XSD.attributeDecl->")
                                             nameOfSimple
                                             (attr_simpleType ad))
                                      XName
                                      (theType n))
                               (comment  (attr_annotation ad))
        Right ref -> case Map.lookup ref (env_attribute env) of
                       Nothing -> error $ "bad attribute reference "
                                          ++printableName ref
                       Just a' -> attributeDecl a'

    attrgroup :: XSD.AttrGroup -> [Haskell.Attribute]
    attrgroup g = case attrgroup_nameOrRef g of
        Left  n   -> concatMap (either attributeDecl attrgroup)
                               (attrgroup_stuff g)
        Right ref -> case Map.lookup ref (env_attrgroup env) of
                       Nothing -> error $ "bad attribute group reference "
                                          ++printableName ref
                       Just g' -> attrgroup g'

    group :: XSD.Group -> [Haskell.HighLevelDecl]
    group g = case group_nameOrRef g of
        Left  n   -> singleton $ Haskell.Group (xname n)
                                   ({-elems-}maybe (error "XSD.group->")
                                                   choiceOrSeq
                                                   (group_stuff g))
                                   (comment (group_annotation g))
        Right ref -> case Map.lookup ref (env_group env) of
                       Nothing -> error $ "bad group reference "
                                          ++printableName ref
                       Just g' -> group g'

    particleAttrs (PA part attrs _) = -- ignoring AnyAttr for now
        (particle part, concatMap (either attributeDecl attrgroup) attrs)

    particle Nothing          = []
    particle (Just (Left cs)) = choiceOrSeq cs
    particle (Just (Right g)) = let [Haskell.Group _ es _] = group g in es

    choiceOrSeq (XSD.All      ann eds)   = error "nyi All"
    choiceOrSeq (XSD.Choice   ann _ ees) = error "nyi Choice"
    choiceOrSeq (XSD.Sequence ann _ ees) = concatMap elementEtc ees

    elementEtc (HasElement ed) = [elementDecl ed]
    elementEtc (HasGroup g)    = let [Haskell.Group _ es _] = group g in es
    elementEtc (HasCS cs)      = choiceOrSeq cs
 -- elementEtc (HasAny a)      = any a


comment :: Annotation -> Comment
comment (Documentation s) = Just s
comment (AppInfo s)       = Just s
comment (NoAnnotation _)  = Nothing

xname :: String -> XName
xname = XName . N

nameOfSimple :: SimpleType -> XName
nameOfSimple (Primitive prim)     = XName . xsd. show $ prim
nameOfSimple (Restricted n _ _ _) = xname n
nameOfSimple (ListOf n _ _ _)     = xname n -- ("["++n++"]")
nameOfSimple (UnionOf n _ _ _)    = xname n -- return to this

nameOfSimple' :: SimpleType -> XName
nameOfSimple' (Primitive prim)     = XName . xsd. show $ prim
nameOfSimple' (Restricted _ s _ _) = nameOfSimple s
nameOfSimple' (ListOf _ s _ _)     = xname ("["++show (nameOfSimple s)++"]")
nameOfSimple' (UnionOf n ss _ _)   = xname n -- return to this

singleton :: a -> [a]
singleton = (:[])

{-
contentInfo :: Maybe (Either SimpleType ComplexType) -> ([Element],[Attribute])
contentInfo Nothing  = ([],[])
contentInfo (Just e) = either simple complex
  where
    simple  :: SimpleType  -> ([Element],[Attribute])
    complex :: ComplexType -> ([Element],[Attribute])
    simple st = ([], ?
-}
