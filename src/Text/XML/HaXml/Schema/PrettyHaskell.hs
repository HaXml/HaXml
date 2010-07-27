module Text.XML.HaXml.Schema.PrettyHaskell
  ( ppComment
  , ppHighLevelDecl
  , ppHighLevelDecls
  ) where

import Text.XML.HaXml.Types (QName(..),Namespace(..))
import Text.XML.HaXml.Schema.HaskellTypeModel
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
    (c:cs) = lines s

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
ppHighLevelDecls :: [HighLevelDecl] -> Doc
ppHighLevelDecls hs = vcat (intersperse (text " ") (map ppHighLevelDecl hs))

-- | Convert a single Haskell HighLevelDecl into Haskell source text.
ppHighLevelDecl :: HighLevelDecl -> Doc

ppHighLevelDecl (NamedSimpleType t s comm) =
    ppComment Before comm
    $$ text "type" <+> ppConId t <+> text "=" <+> ppConId s

ppHighLevelDecl (RestrictSimpleType t s comm) =
    ppComment Before comm
    $$ text "newtype" <+> ppConId t <+> text "=" <+> ppConId t <+> ppConId s
    $$ text "instance Restricts" <+> ppConId t <+> ppConId s <+> text "where"
        $$ nest 4 (text "restricts (" <> ppConId t <+> text "x) = x")
    $$ text "instance SchemaType" <+> ppConId t <+> text "where"
        $$ nest 4 (text "parseSchemaType = fmap " <+> ppConId t <+>
                   text ". parseSchemaType")
		-- XXX should enforce the restriction.

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

ppHighLevelDecl (ElementsAttrs t es as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId t <+> text "=" <+> ppConId t <+> ppFields es as
    $$ text "instance SchemaType" <+> ppConId t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "e <- element [s]"
                            $$ text "commit $ do"
                            $$ nest 2 (vcat (zipWith doAttr as [0..])
                                      $$ text "interior e $ do"
                                      $$ nest 2 (vcat (zipWith doElem es [0..])
                                                $$ text "return $" <+>
                                                   returnValue as es))))
  where
    doAttr a n = (text "a"<>text (show n)) <+> text "<- getAttribute \""
                                           <+> ppXName (attr_name a)
                                           <+> text "\" e"
    doElem e n = (text "e"<>text (show n)) <+> text "<-"
                                           <+> ppModifier (elem_modifier e)
                                           <+> text "parseSchemaType \""
                                           <+> ppXName (elem_name e)
                                           <+> text "\""
    ppModifier Single    = empty
    ppModifier Optional  = text "optional $"
    ppModifier (Range o) = text "between" <+> text (show o) <+>  text "$"

    returnValue as es = ppConId t <+>
                        hsep ([text ("a"++show n) | n <- [0..length as]]
                              ++ [text ("e"++show n) | n <- [0..length es]])

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

ppHighLevelDecl (ExtendComplexType t s es as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId t <+> text "=" <+> ppConId t <+> ppConId s
                                               <+> ppAuxConId t
    $$ text "data" <+> ppAuxConId t <+> text "=" <+> ppAuxConId t
                                               <+> ppFields es as
    $$ text "instance Extension" <+> ppConId t <+> ppConId s <+> ppAuxConId t
                                  <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId t <> text " s e) = s"
                   $$ text "extension (" <> ppConId t <> text " s e) = e")

ppHighLevelDecl (XSDInclude m comm) =
    text "import" <+> ppModId m <+> ppComment After comm

ppHighLevelDecl (XSDImport m comm) =
    text "import" <+> ppModId m <+> ppComment After comm

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
                      <+> ppModifier (elem_modifier e) (ppConId (elem_type e))
                      <+> ppComment After (elem_comment e)

-- | Generate a single named field from an attribute.
ppAttribute :: Attribute -> Doc
ppAttribute a = ppVarId (attr_name a) <+> text "::"
                        <+> ppConId (attr_type a)
                        <+> ppComment After (attr_comment a)

-- | Generate a list or maybe type name.
ppModifier :: Modifier -> Doc -> Doc
ppModifier Single d    = d
ppModifier Optional  d = text "Maybe" <+> d
ppModifier (Range o) d = text "[" <> d <> text "]"

