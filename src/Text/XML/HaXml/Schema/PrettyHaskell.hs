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
ppModId, ppConId, ppVarId, ppAuxConId, ppAuxVarId :: XName -> Doc
ppModId = ppHName . modid simpleNameConverter
ppConId = ppHName . conid simpleNameConverter
ppVarId = ppHName . varid simpleNameConverter
ppAuxConId = ppHName . auxconid simpleNameConverter
ppAuxVarId = ppHName . auxvarid simpleNameConverter

-- | Convert a whole document from HaskellTypeModel to Haskell source text.
ppModule :: Module -> Doc
ppModule m =
    text "module" <+> ppModId (module_name m)
    $$ nest 2 (text "( module" <+> ppModId (module_name m)
              $$ vcat (map (\(XSDInclude ex com)->
                               ppComment Before com
                               $$ text ", module" <+> ppModId ex)
                           (module_re_exports m))
              $$ text ") where")
    $$ text " "
    $$ text "import Text.XML.HaXml.Schema.Schema as Xsd"
    $$ vcat (map ppHighLevelDecl (module_re_exports m ++ module_import_only m))
    $$ text " "
    $$ ppHighLevelDecls (module_decls m)

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
ppHighLevelDecls :: [Decl] -> Doc
ppHighLevelDecls hs = vcat (intersperse (text " ") (map ppHighLevelDecl hs))

-- | Convert a single Haskell Decl into Haskell source text.
ppHighLevelDecl :: Decl -> Doc

ppHighLevelDecl (NamedSimpleType t s comm) =
    ppComment Before comm
    $$ text "type" <+> ppConId t <+> text "=" <+> ppConId s

ppHighLevelDecl (RestrictSimpleType t s r comm) =
    ppComment Before comm
    $$ text "newtype" <+> ppConId t <+> text "=" <+> ppConId t <+> ppConId s
    $$ text "instance Restricts" <+> ppConId t <+> ppConId s <+> text "where"
        $$ nest 4 (text "restricts (" <> ppConId t <+> text "x) = x")
    $$ text "instance SimpleType" <+> ppConId t <+> text "where"
        $$ nest 4 (text "acceptingParser = undefined")
    $$ text "instance SchemaType" <+> ppConId t <+> text "where"
        $$ nest 4 (text "parseSchemaType = fmap " <+> ppConId t <+>
                   text ". parseAccepting parseSchemaType")
		-- XXX should enforce the restriction.  (?)

ppHighLevelDecl (ExtendSimpleType t s as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId t <+> text "=" <+> ppConId t <+> ppConId s
                                               <+> (ppConId t<>text "Fields")
    $$ text "data" <+> (ppConId t<>text "Fields") <+> text "=" <+> text "..."
    $$ text "instance Extension" <+> ppConId t <+> ppConId s
                                  <+> (ppConId t<>text "Fields")
                                  <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId t <> text " s e) = s"
                   $$ text "extension (" <> ppConId t <> text " s e) = e")

ppHighLevelDecl (UnionSimpleTypes t sts comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId t <+> text "=" <+> ppConId t
    $$ text "-- Placeholder for a Union type, not yet implemented."

ppHighLevelDecl (ElementsAttrs t es as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId t <+> text "=" <+> ppConId t
        $$ nest 8 (ppFields es as)
    $$ text "instance SchemaType" <+> ppConId t <+> text "where"
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
    returnValue [] = ppConId t
    returnValue as = parens (ppConId t <+>
                             hsep [text ("a"++show n) | n <- [0..length as-1]])

ppHighLevelDecl (ElementOfType e) =
    (text "element" <> ppVarId (elem_name e)) <+> text "::"
        <+> text "SchemaParser" <+> ppConId (elem_type e)
    $$
    (text "element" <> ppVarId (elem_name e)) <+> text "="
        <+> (text "parseSchemaType \"" <> ppXName (elem_name e)  <> text "\"")

ppHighLevelDecl (Choice t es comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId t <+> text "=" <+>
        nest 4 (vcat (zipWith choices es [0..]))
  where
    choices e n = (ppConId t <> text (show n)) <+> ppConId (elem_type e)

ppHighLevelDecl (Group t es comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId t <+> text "="
                    <+> ppConId t <+> hcat (map (ppConId . elem_type) es)

ppHighLevelDecl (RestrictComplexType t s comm) =
    ppComment Before comm
    $$ text "newtype" <+> ppConId t <+> text "=" <+> ppConId t <+> ppConId s
    $$ text "-- plus different (more restrictive) parser"
    $$ text "instance Restricts" <+> ppConId t <+> ppConId s <+> text "where"
        $$ nest 4 (text "restricts (" <> ppConId t <+> text "x) = x")
    $$ text "instance SchemaType" <+> ppConId t <+> text "where"
        $$ nest 4 (text "parseSchemaType = fmap " <+> ppConId t <+>
                   text ". parseSchemaType")
		-- XXX should enforce the restriction.

{-
ppHighLevelDecl (ExtendComplexType t s es as comm)
    | length es + length as = 1 =
    ppComment Before comm
    $$ text "data" <+> ppConId t <+> text "=" <+> ppConId t <+> ppConId s
                                               <+> ppFields es as
    $$ text "instance Extension" <+> ppConId t <+> ppConId s <+> ppAuxConId t
                                  <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId t <> text " s e) = s"
                   $$ text "extension (" <> ppConId t <> text " s e) = e")
-}
ppHighLevelDecl (ExtendComplexType t s es as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId t <+> text "=" <+> ppConId t <+> ppConId s
                                              <+> ppAuxConId t
    $$ text "data" <+> ppAuxConId t <+> text "=" <+> ppAuxConId t
                                                 $$ nest 8 (ppFields es as)
    $$ text "instance SchemaType" <+> ppConId t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "e <- element [s]"
                            $$ text "commit $"
                               <+> text "interior e $ contentsOfSchemaType")
                  $$ text "contentsOfSchemaType = return" <+> ppConId t
                            $$ nest 4 (text "<*> contentsOfSchemaType"
                                      $$ text "<*> contentsOfExtension"))
    $$ text "instance Extension" <+> ppConId t <+> ppConId s <+> ppAuxConId t
                                  <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId t <> text " s e) = s"
                   $$ text "extension (" <> ppConId t <> text " s e) = e"
                   $$ text "contentsOfExtension = return" <+> ppAuxConId t
                   $$ nest 4 (vcat (map ppElem es)))

ppHighLevelDecl (XSDInclude m comm) =
    ppComment After comm
    $$ text "import" <+> ppModId m

ppHighLevelDecl (XSDImport m comm) =
    ppComment After comm
    $$ text "import" <+> ppModId m

ppHighLevelDecl (XSDComment comm) =
    ppComment Before comm


--------------------------------------------------------------------------------

-- | Generate named fiedls from elements and attributes.
ppFields :: [Element] -> [Attribute] -> Doc
ppFields es as | null es && null as = empty
ppFields es as =  vcat ( text "{" <+> head fields
                       : map (text "," <+>) (tail fields)
                       ++ [text "}"] )
  where
    fields = map ppAttribute as ++  map ppElement es

-- | Generate a single named field from an element.
ppElement :: Element -> Doc
ppElement e = ppVarId (elem_name e) <+> text "::"
                      <+> ppTypeModifier (elem_modifier e)
                                         (ppConId (elem_type e))
              $$ ppComment After (elem_comment e)

-- | Generate a single named field from an attribute.
ppAttribute :: Attribute -> Doc
ppAttribute a = ppVarId (attr_name a) <+> text "::"
                        <+> ppConId (attr_type a)
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

