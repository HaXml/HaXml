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

    simple env s@(Restricted _ (Just n) _ _)
                                 = env{env_type=Map.insert (N n) (Left s)
                                                           (env_type env)}
    simple env s@(ListOf _ (Just n) _ _)
                                 = env{env_type=Map.insert (N n) (Left s)
                                                           (env_type env)}
    simple env s@(UnionOf _ (Just n) _ _ _)
                                 = env{env_type=Map.insert (N n) (Left s)
                                                           (env_type env)}
    simple env   _               = env

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

convert :: Environment -> Schema -> [Decl]
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
    simple s@(Restricted a n f r)
        | (Just enums) <- isEnumeration s
                                = [EnumSimpleType 
                                       (maybe (error "missing Name") xname n)
                                       enums (comment a) ]
        | otherwise             = [RestrictSimpleType
                                       (maybe (error "missing Name") xname n)
                                       (maybe (xname "unknownSimple") XName
                                                             (restrict_base r))
                                       (mkRestrict r)
                                       (comment a)]
    simple (ListOf a n f t)     = error "Not yet implemented: ListOf simpleType"
                              --  [NamedSimpleType    (xname n) (nameOfSimple s)
                              --                      (comment a)]
    simple s@(UnionOf a n f u m)
        | (Just enums) <- isEnumeration s
                                = [EnumSimpleType 
                                       (maybe (error "missing Name") xname n)
                                       enums (comment a) ]
        | otherwise             = [UnionSimpleTypes
                                       (maybe (error "missing Name") xname n)
                                       (map (xname . printableName) m) -- XXX ignores content 'u'
                                       (comment a)]

    isEnumeration :: SimpleType -> Maybe [(XName,Comment)]
    isEnumeration (Primitive _)        = Nothing
    isEnumeration (ListOf _ _ _ _)     = Nothing
    isEnumeration (Restricted _ _ _ r) =
        case r of
            RestrictSim1 ann base r1  -> Nothing
            RestrictType _ _ _ facets ->
                let enum = [ (xname v, comment ann)
                           | (Facet UnorderedEnumeration ann v _) <- facets ]
                in if null enum then Nothing else Just enum
    isEnumeration (UnionOf _ _ _ u ms) =
        squeeze [] ( flip map ms (\m-> case Map.lookup m (env_type env) of
                                         Just (Left s)-> isEnumeration s
                                         _            -> Nothing)
                     ++ map isEnumeration u )
        where squeeze _  (Nothing:_)    = Nothing
              squeeze xs (Just ys:rest) = squeeze (xs++ys) rest
              squeeze xs []             = Just xs

    complex ct =
      let n = xname $ fromMaybe ("errorMissingName") (complex_name ct)
      in singleton $
      case complex_content ct of
        c@SimpleContent{}  ->
            case ci_stuff c of
                Left r  ->
                    RestrictSimpleType n ({-simple-}xname $ "Unimplemented") []
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

    topElementDecl :: XSD.ElementDecl -> [Haskell.Decl]
    topElementDecl ed = case elem_nameOrRef ed of
        Left  n   -> singleton $
                     case theType n of
                       Nothing ->
                       --error "Not implemented: contentInfo on topElementDecl"
                         let (es,as) = contentInfo (elem_content ed) in
                         ElementsAttrs ({-name-}xname $ theName n)
                                       ({-elems-}es)
                                       ({-attrs-}as)
                                       (comment (elem_annotation ed))
                       Just t ->
                         ElementOfType Element{ elem_name = xname $ theName n
                                              , elem_type = XName t
                                              , elem_modifier = Single -- XXX
                                              , elem_locals  = []
                                              , elem_comment =
                                                  (comment (elem_annotation ed))
                                              }
        Right ref -> case Map.lookup ref (env_element env) of
		       Nothing -> error $ "<topElementDecl> unknown element reference "
					  ++printableName ref
		       Just e' -> topElementDecl e'

    elementDecl :: XSD.ElementDecl -> Haskell.Element
    elementDecl ed = case elem_nameOrRef ed of
        Left  n   -> Element ({-name-}xname $ theName n)
                             ({-type-}maybe (xname "unknown") XName $ theType n)
                             ({-modifier-}Haskell.Range $ elem_occurs ed)
                             [] -- internal Decl
                             (comment (elem_annotation ed))
        Right ref -> case Map.lookup ref (env_element env) of
                       Nothing -> Element (XName ref)
                                          (xname "unknown")
                                          (Haskell.Single) [] Nothing
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
                       Nothing -> error $ "<attributeDecl> unknown attribute reference "
                                          ++printableName ref
                       Just a' -> attributeDecl a'

    attrgroup :: XSD.AttrGroup -> [Haskell.Attribute]
    attrgroup g = case attrgroup_nameOrRef g of
        Left  n   -> concatMap (either attributeDecl attrgroup)
                               (attrgroup_stuff g)
        Right ref -> case Map.lookup ref (env_attrgroup env) of
                       Nothing -> error $ "unknown attribute group reference "
                                          ++printableName ref
                       Just g' -> attrgroup g'

    group :: XSD.Group -> [Haskell.Decl]
    group g = case group_nameOrRef g of
        Left  n   -> let ({-highs,-}es) = choiceOrSeq (fromMaybe (error "XSD.group")
                                                             (group_stuff g))
                     in {-highs ++-} singleton $
                           Haskell.Group (xname n) es
                                         (comment (group_annotation g))
        Right ref -> case Map.lookup ref (env_group env) of
                  --   Nothing -> error $ "bad group reference "
                  --                      ++printableName ref
                       Nothing -> singleton $
                                  Haskell.Group (xname ("unknown-group-"++printableName ref)) []
                                                (comment (group_annotation g))
                       Just g' -> group g'

    particleAttrs :: ParticleAttrs -> ([Haskell.Element],[Haskell.Attribute])
    particleAttrs (PA part attrs _) = -- ignoring AnyAttr for now
        (particle part, concatMap (either attributeDecl attrgroup) attrs)

    particle :: Particle -> [Haskell.Element] -- XXX fix to ret Decls
    particle Nothing          = []
    particle (Just (Left cs)) = {-snd $-} choiceOrSeq cs
    particle (Just (Right g)) = let [Haskell.Group _ es _] = group g in es

--  choiceOrSeq :: ChoiceOrSeq -> ([Haskell.Decl],[Haskell.Element])
    choiceOrSeq :: ChoiceOrSeq -> [Haskell.Element]
    choiceOrSeq (XSD.All      ann eds)   = [] -- error "nyi All"
    choiceOrSeq (XSD.Choice   ann _ ees) = [] -- error "nyi Choice"
    choiceOrSeq (XSD.Sequence ann _ ees) = concatMap elementEtc ees

    elementEtc :: ElementEtc -> [Haskell.Element]
    elementEtc (HasElement ed) = [elementDecl ed]
    elementEtc (HasGroup g)    = let [Haskell.Group _ es _] = group g in es
    elementEtc (HasCS cs)      = choiceOrSeq cs
    elementEtc (HasAny a)      = [] -- XXX clearly wrong
 -- elementEtc (HasAny a)      = any a

    contentInfo :: Maybe (Either SimpleType ComplexType)
                   -> ([Haskell.Element],[Haskell.Attribute])
    contentInfo Nothing  = ([],[])
    contentInfo (Just e) = either simple complex e
      where
        simple  :: SimpleType  -> ([Element],[Attribute])
        complex :: ComplexType -> ([Element],[Attribute])
        simple _         = ([], [])  -- XXX clearly wrong
     -- simple (Primitive p)        = ([], [])  -- XXX clearly wrong
     -- simple (Restricted n _ _ _) =
        complex ct = case complex_content ct of
                       SimpleContent{}  -> ([],[]) -- XXX clearly wrong
                       ComplexContent{} -> ([],[]) -- XXX clearly wrong
                       ThisType pa      -> particleAttrs pa


comment :: Annotation -> Comment
comment (Documentation s) = Just s
comment (AppInfo s)       = Just s
comment (NoAnnotation _)  = Nothing

xname :: String -> XName
xname = XName . N

nameOfSimple :: SimpleType -> XName
nameOfSimple (Primitive prim)            = XName . xsd . show $ prim
nameOfSimple (Restricted _ (Just n) _ _) = xname n
nameOfSimple (ListOf _ (Just n) _ _)     = xname n -- ("["++n++"]")
nameOfSimple (UnionOf _ (Just n) _ _ _)  = xname n -- return to this
nameOfSimple s                           = xname "String" -- anonymous simple

mkRestrict :: XSD.Restriction -> [Haskell.Restrict]
mkRestrict (RestrictSim1 ann base r1) =
        error "Not yet implemented: Restriction1 on simpletype"
mkRestrict (RestrictType _ _ _ facets) =
    (let occurs = [ (f,ann,v)  | (Facet f ann v _) <- facets
                               , f `elem` [OrderedBoundsMinIncl
                                          ,OrderedBoundsMinExcl
                                          ,OrderedBoundsMaxIncl
                                          ,OrderedBoundsMaxExcl] ]
     in if null occurs then []
        else [Haskell.RangeR (foldl consolidate (Occurs Nothing Nothing) occurs)
                             (comment $ foldr mappend mempty
                                              [ ann | (_,ann,_) <- occurs])]
    ) ++
    [ Haskell.Pattern v (comment ann)
              | (Facet UnorderedPattern ann v _) <- facets ]
    ++
    (let enum = [ (v,comment ann)
                | (Facet UnorderedEnumeration ann v _) <- facets ]
     in if null enum then []
                     else [Haskell.Enumeration enum]
    ) ++
    (let occurs = [ (f,ann,v)  | (Facet f ann v _) <- facets
                               , f `elem` [UnorderedLength
                                          ,UnorderedMaxLength
                                          ,UnorderedMinLength] ]
     in if null occurs then []
        else [Haskell.StrLength
                 (foldl consolidate (Occurs Nothing Nothing) occurs)
                 (comment $ foldr mappend mempty [ ann | (_,ann,_) <- occurs])]
    )

singleton :: a -> [a]
singleton = (:[])

-- | Consolidate a Facet occurrence into a single Occurs value.
consolidate :: Occurs -> (FacetType,Annotation,String) -> Occurs
consolidate (Occurs min max) (OrderedBoundsMinIncl,_,n) =
             Occurs (Just (read n)) max
consolidate (Occurs min max) (OrderedBoundsMinExcl,_,n) =
             Occurs (Just ((read n)+1)) max
consolidate (Occurs min max) (OrderedBoundsMaxIncl,_,n) =
             Occurs min (Just (read n))
consolidate (Occurs min max) (OrderedBoundsMaxExcl,_,n) =
             Occurs min (Just ((read n)-1))
consolidate (Occurs min max) (UnorderedLength,_,n) =
             Occurs (Just (read n)) (Just (read n))
consolidate (Occurs min max) (UnorderedMinLength,_,n) =
             Occurs (Just (read n)) max
consolidate (Occurs min max) (UnorderedMaxLength,_,n) =
             Occurs min (Just (read n))

