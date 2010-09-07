module Text.XML.HaXml.Schema.PrettyHaskell
  ( ppComment
  , ppModule
  , ppHighLevelDecl
  , ppHighLevelDecls
  ) where

import Text.XML.HaXml.Types (QName(..),Namespace(..))
import Text.XML.HaXml.Schema.HaskellTypeModel
import Text.XML.HaXml.Schema.XSDTypeModel (Occurs(..))
import Text.XML.HaXml.Schema.NameConversion
import Text.PrettyPrint.HughesPJ as PP

import List (intersperse)

data CommentPosition = Before | After

-- | Generate aligned haddock-style documentation.
--   (but without escapes in comment text yet)
ppComment :: CommentPosition -> Comment -> Doc
ppComment _   Nothing  = empty
ppComment pos (Just s) =
    text "--" <+> text (case pos of Before -> "|"; After -> "^") <+> text c
    $$
    vcat (map (\x-> text "--  " <+> text x) cs)
  where
    (c:cs) = lines (paragraph 60 s)

-- | Pretty-print a Haskell-style name.
ppHName :: HName -> Doc
ppHName (HName x) = text x

-- | Pretty-print an XML-style name.
ppXName :: XName -> Doc
ppXName (XName (N x))     = text x
ppXName (XName (QN ns x)) = text (nsPrefix ns) <> text ":" <> text x

-- | Some different ways of using a Haskell identifier.
ppModId, ppConId, ppVarId, ppAuxConId, ppAuxVarId
    :: NameConverter -> XName -> Doc
ppModId nx = ppHName . modid nx
ppConId nx = ppHName . conid nx
ppVarId nx = ppHName . varid nx
ppAuxConId nx = ppHName . auxconid nx
ppAuxVarId nx = ppHName . auxvarid nx

-- | Convert a whole document from HaskellTypeModel to Haskell source text.
ppModule :: NameConverter -> Module -> Doc
ppModule nx m =
    text "module" <+> ppModId nx (module_name m)
    $$ nest 2 (text "( module" <+> ppModId nx (module_name m)
              $$ vcat (map (\(XSDInclude ex com)->
                               ppComment Before com
                               $$ text ", module" <+> ppModId nx ex)
                           (module_re_exports m))
              $$ text ") where")
    $$ text " "
    $$ text "import Text.XML.HaXml.Schema.Schema as Xsd"
    $$ vcat (map (ppHighLevelDecl nx)
                 (module_re_exports m ++ module_import_only m))
    $$ text " "
    $$ ppHighLevelDecls nx (module_decls m)

-- | Generate a fragmentary parser for an attribute.
ppAttr a n = (text "a"<>text (show n)) <+> text "<- getAttribute \""
                                       <> ppXName (attr_name a)
                                       <> text "\" e"
-- | Generate a fragmentary parser for an element.
ppElem e = text "<*>" <+> ppElemModifier (elem_modifier e)
                                         (text "parseSchemaType \""
                                           <> ppXName (elem_name e)
                                           <> text "\"")

-- | Convert multiple HaskellTypeModel Decls to Haskell source text.
ppHighLevelDecls :: NameConverter -> [Decl] -> Doc
ppHighLevelDecls nx hs = vcat (intersperse (text " ")
                                           (map (ppHighLevelDecl nx) hs))

-- | Convert a single Haskell Decl into Haskell source text.
ppHighLevelDecl :: NameConverter -> Decl -> Doc

ppHighLevelDecl nx (NamedSimpleType t s comm) =
    ppComment Before comm
    $$ text "type" <+> ppConId nx t <+> text "=" <+> ppConId nx s

ppHighLevelDecl nx (RestrictSimpleType t s r comm) =
    ppComment Before comm
    $$ text "newtype" <+> ppConId nx t <+> text "="
                      <+> ppConId nx t <+> ppConId nx s
    $$ text "instance Restricts" <+> ppConId nx t <+> ppConId nx s
                      <+> text "where"
        $$ nest 4 (text "restricts (" <> ppConId nx t <+> text "x) = x")
    $$ text "instance SimpleType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "acceptingParser = undefined")
    $$ text "instance SchemaType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType = fmap " <+> ppConId nx t <+>
                   text ". parseAccepting parseSchemaType")
		-- XXX should enforce the restriction.  (?)
    $$ text "-- The restrictions are:" <+> hsep (map ppRestrict r)
  where
    ppRestrict (RangeR occ comm)     = text "(RangeR"
                                         <+> ppElemModifier (Range occ) empty
                                         <>  text ")"
    ppRestrict (Pattern regexp comm) = text "(Pattern)"
    ppRestrict (Enumeration items)   = text "(Enumeration"
                                         <+> hsep (map (text . fst) items)
                                         <>  text ")"

ppHighLevelDecl nx (ExtendSimpleType t s as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "="
                                    <+> ppConId nx t <+> ppConId nx s
                                    <+> (ppConId nx t<>text "Fields")
    $$ text "data" <+> (ppConId nx t<>text "Fields") <+> text "=" <+> text "..."
    $$ text "instance Extension" <+> ppConId nx t <+> ppConId nx s
                                  <+> (ppConId nx t<>text "Fields")
                                  <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId nx t <> text " s e) = s"
                   $$ text "extension (" <> ppConId nx t <> text " s e) = e")

ppHighLevelDecl nx (UnionSimpleTypes t sts comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "=" <+> ppConId nx t
    $$ text "-- Placeholder for a Union type, not yet implemented."

ppHighLevelDecl nx (EnumSimpleType t [] comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t
ppHighLevelDecl nx (EnumSimpleType t (i:is) comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t
        $$ nest 4 ( vcat ( text "=" <+> item i
                         : map (\i-> text "|" <+> item i) is)
                  $$ text "deriving (Eq,Show,Enum)" )
    $$ text "instance SchemaType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = undefined")
  where
    item (i,c) = (ppConId nx t <> text "_" <> ppConId nx i)
                 <+> ppComment After c

ppHighLevelDecl nx (ElementsAttrs t es as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "=" <+> ppConId nx t
        $$ nest 8 (ppFields nx es as)
    $$ text "instance SchemaType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "e <- element [s]"
                            $$ text "commit $ do"
                            $$ nest 2
                                  (vcat (zipWith ppAttr as [0..])
                                  $$ text "interior e $ contentsOfSchemaType"))
                  $$ text "contentsOfSchemaType = return" <+> returnValue as
                                      $$ nest 4 (vcat (map ppElem es))
                  )
  where
    returnValue [] = ppConId nx t
    returnValue as = parens (ppConId nx t <+>
                             hsep [text ("a"++show n) | n <- [0..length as-1]])

ppHighLevelDecl nx (ElementOfType e) =
    (text "element" <> ppVarId nx (elem_name e)) <+> text "::"
        <+> text "SchemaParser" <+> ppConId nx (elem_type e)
    $$
    (text "element" <> ppVarId nx (elem_name e)) <+> text "="
        <+> (text "parseSchemaType \"" <> ppXName (elem_name e)  <> text "\"")

ppHighLevelDecl nx (Choice t es comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "=" <+>
        nest 4 (vcat (zipWith choices es [0..]))
  where
    choices e n = (ppConId nx t <> text (show n)) <+> ppConId nx (elem_type e)

ppHighLevelDecl nx (Group t es comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "="
                   <+> ppConId nx t <+> hcat (map (ppConId nx . elem_type) es)

ppHighLevelDecl nx (RestrictComplexType t s comm) =
    ppComment Before comm
    $$ text "newtype" <+> ppConId nx t <+> text "="
                                       <+> ppConId nx t <+> ppConId nx s
    $$ text "-- plus different (more restrictive) parser"
    $$ text "instance Restricts" <+> ppConId nx t <+> ppConId nx s
                                 <+> text "where"
        $$ nest 4 (text "restricts (" <> ppConId nx t <+> text "x) = x")
    $$ text "instance SchemaType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType = fmap " <+> ppConId nx t <+>
                   text ". parseSchemaType")
		-- XXX should enforce the restriction.

{-
ppHighLevelDecl nx (ExtendComplexType t s es as comm)
    | length es + length as = 1 =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "="
                                    <+> ppConId nx t <+> ppConId nx s
                                    <+> ppFields nx es as
    $$ text "instance Extension" <+> ppConId nx t <+> ppConId nx s
                                 <+> ppAuxConId nx t <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId nx t <> text " s e) = s"
                   $$ text "extension (" <> ppConId nx t <> text " s e) = e")
-}
ppHighLevelDecl nx (ExtendComplexType t s es as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "="
                                    <+> ppConId nx t <+> ppConId nx s
                                    <+> ppAuxConId nx t
    $$ text "data" <+> ppAuxConId nx t <+> text "=" <+> ppAuxConId nx t
                                                 $$ nest 8 (ppFields nx es as)
    $$ text "instance SchemaType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "e <- element [s]"
                            $$ text "commit $"
                               <+> text "interior e $ contentsOfSchemaType")
                  $$ text "contentsOfSchemaType = return" <+> ppConId nx t
                            $$ nest 4 (text "<*> contentsOfSchemaType"
                                      $$ text "<*> contentsOfExtension"))
    $$ text "instance Extension" <+> ppConId nx t <+> ppConId nx s
                                 <+> ppAuxConId nx t <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId nx t <> text " s e) = s"
                   $$ text "extension (" <> ppConId nx t <> text " s e) = e"
                   $$ text "contentsOfExtension = return" <+> ppAuxConId nx t
                   $$ nest 4 (vcat (map ppElem es)))

ppHighLevelDecl nx (XSDInclude m comm) =
    ppComment After comm
    $$ text "import" <+> ppModId nx m

ppHighLevelDecl nx (XSDImport m comm) =
    ppComment After comm
    $$ text "import" <+> ppModId nx m

ppHighLevelDecl nx (XSDComment comm) =
    ppComment Before comm


--------------------------------------------------------------------------------

-- | Generate named fiedls from elements and attributes.
ppFields :: NameConverter -> [Element] -> [Attribute] -> Doc
ppFields nx es as | null es && null as = empty
ppFields nx es as =  vcat ( text "{" <+> head fields
                          : map (text "," <+>) (tail fields)
                          ++ [text "}"] )
  where
    fields = map (ppAttribute nx) as ++  map (ppElement nx) es

-- | Generate a single named field from an element.
ppElement :: NameConverter -> Element -> Doc
ppElement nx e = ppVarId nx (elem_name e) <+> text "::"
                      <+> ppTypeModifier (elem_modifier e)
                                         (ppConId nx (elem_type e))
                 $$ ppComment After (elem_comment e)

-- | Generate a single named field from an attribute.
ppAttribute :: NameConverter -> Attribute -> Doc
ppAttribute nx a = ppVarId nx (attr_name a) <+> text "::"
                            <+> ppConId nx (attr_type a)
                   $$ ppComment After (attr_comment a)

-- | Generate a list or maybe type name.
ppTypeModifier :: Modifier -> Doc -> Doc
ppTypeModifier Single d    = d
ppTypeModifier Optional  d = text "Maybe" <+> d
ppTypeModifier (Range (Occurs Nothing Nothing))  d = d
ppTypeModifier (Range (Occurs (Just 0) Nothing)) d = text "Maybe" <+> d
ppTypeModifier (Range (Occurs _ _))              d = text "[" <> d <> text "]"

-- | Generate a parser for a list or Maybe value.
ppElemModifier Single    doc = doc
ppElemModifier Optional  doc = text "optional" <+> parens doc
ppElemModifier (Range (Occurs Nothing Nothing))  doc = doc
ppElemModifier (Range (Occurs (Just 0) Nothing)) doc = text "optional"
                                                       <+> parens doc
ppElemModifier (Range o) doc = text "between" <+> parens (text (show o))
                                              <+> parens doc

-- | Split long lines of comment text into a paragraph with a maximum width.
paragraph :: Int -> String -> String
paragraph n s = go n (words s)
    where go i []     = []
          go i (x:xs) | len<i     =       x++" "++go (i-len-1) xs
                      | otherwise = "\n"++x++" "++go (n-len-1) xs
              where len = length x

