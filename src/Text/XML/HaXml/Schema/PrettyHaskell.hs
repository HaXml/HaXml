module Text.XML.HaXml.Schema.PrettyHaskell
  ( ppComment
  , ppModule
  , ppHighLevelDecl
  , ppHighLevelDecls
  , ppvList
  ) where

import Text.XML.HaXml.Types (QName(..),Namespace(..))
import Text.XML.HaXml.Schema.HaskellTypeModel
import Text.XML.HaXml.Schema.XSDTypeModel (Occurs(..))
import Text.XML.HaXml.Schema.NameConversion
import Text.PrettyPrint.HughesPJ as PP

import List (intersperse)

-- | Vertically pretty-print a list of things, with open and close brackets,
--   and separators.
ppvList :: String -> String -> String -> (a->Doc) -> [a] -> Doc
ppvList open sep close pp []     = text open <> text close
ppvList open sep close pp (x:xs) = text open <+> pp x
                                   $$ vcat (map (\y-> text sep <+> pp y) xs)
                                   $$ text close

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
ppFieldId  nx = \t-> ppHName . fieldid nx t

-- | Convert a whole document from HaskellTypeModel to Haskell source text.
ppModule :: NameConverter -> Module -> Doc
ppModule nx m =
    text "{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}"
    $$ text "module" <+> ppModId nx (module_name m)
    $$ nest 2 (text "( module" <+> ppModId nx (module_name m)
              $$ vcat (map (\(XSDInclude ex com)->
                               ppComment Before com
                               $$ text ", module" <+> ppModId nx ex)
                           (module_re_exports m))
              $$ text ") where")
    $$ text " "
    $$ text "import Text.XML.HaXml.Schema.Schema as Xsd hiding (Reference)"
    $$ vcat (map (ppHighLevelDecl nx)
                 (module_re_exports m ++ module_import_only m))
    $$ text " "
    $$ ppHighLevelDecls nx (module_decls m)

-- | Generate a fragmentary parser for an attribute.
ppAttr :: Attribute -> Int -> Doc
ppAttr a n = (text "a"<>text (show n)) <+> text "<- getAttribute \""
                                       <> ppXName (attr_name a)
                                       <> text "\" e pos"
-- | Generate a fragmentary parser for an element.
ppElem :: Element -> Doc
ppElem e@Element{}
    | elem_byRef e = text "element" <> ppXName (elem_name e)
    | otherwise    = ppElemModifier (elem_modifier e)
                                    (text "parseSchemaType \""
                                     <> ppXName (elem_name e)
                                     <> text "\"")
ppElem e@OneOf{}   = ppElemModifier (elem_modifier e)
                         (text "oneOf" <+> ppvList "[" "," "]"
                                                   (ppOneOf n)
                                                   (zip (elem_oneOf e) [1..n]))
  where
    n = length (elem_oneOf e)
    ppOneOf n (e,i) = text "fmap" <+> text (ordinal i ++"Of"++show n)
                      <+> parens (ppElem e)
    ordinal i | i <= 20   = ordinals!!i
              | otherwise = "Choice" ++ show i
    ordinals = ["Zero","One","Two","Three","Four","Five","Six","Seven","Eight"
               ,"Nine","Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen"
               ,"Sixteen","Seventeen","Eighteen","Nineteen","Twenty"]

-- | Convert multiple HaskellTypeModel Decls to Haskell source text.
ppHighLevelDecls :: NameConverter -> [Decl] -> Doc
ppHighLevelDecls nx hs = vcat (intersperse (text " ")
                                           (map (ppHighLevelDecl nx) hs))

-- | Convert a single Haskell Decl into Haskell source text.
ppHighLevelDecl :: NameConverter -> Decl -> Doc

ppHighLevelDecl nx (NamedSimpleType t s comm) =
    ppComment Before comm
    $$ text "type" <+> ppConId nx t <+> text "=" <+> ppConId nx s
    $$ text "-- No instances required: synonym is isomorphic to the original."

ppHighLevelDecl nx (RestrictSimpleType t s r comm) =
    ppComment Before comm
    $$ text "newtype" <+> ppConId nx t <+> text "="
                      <+> ppConId nx t <+> ppConId nx s
                      <+> text "deriving (Eq,Show)"
    $$ text "instance Restricts" <+> ppConId nx t <+> ppConId nx s
                      <+> text "where"
        $$ nest 4 (text "restricts (" <> ppConId nx t <+> text "x) = x")
    $$ text "instance SchemaType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "e <- element [s]"
                           $$ text "commit $ interior e $ parseSimpleType")
                  )
    $$ text "instance SimpleType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "acceptingParser = fmap" <+> ppConId nx t
                                                 <+> text "acceptingParser"
                   -- XXX should enforce the restrictions somehow.  (?)
                   $$ text "-- XXX should enforce the restrictions somehow?"
                   $$ text "-- The restrictions are:"
                   $$ vcat (map ((text "--     " <+>) . ppRestrict) r))
  where
    ppRestrict (RangeR occ comm)     = text "(RangeR"
                                         <+> ppElemModifier (Range occ) empty
                                         <>  text ")"
    ppRestrict (Pattern regexp comm) = text ("(Pattern "++regexp++")")
    ppRestrict (Enumeration items)   = text "(Enumeration"
                                         <+> hsep (map (text . fst) items)
                                         <>  text ")"
    ppRestrict (StrLength occ comm)  = text "(StrLength"
                                         <+> ppElemModifier (Range occ) empty
                                         <>  text ")"

ppHighLevelDecl nx (ExtendSimpleType t s as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "="
                                    <+> ppConId nx t <+> ppConId nx s
                                    <+> ppConId nx t_attrs
    $$ text "data" <+> ppConId nx t_attrs <+> text "=" <+> ppConId nx t_attrs
        $$ nest 4 (ppFields nx t_attrs [] as)
    $$ text "instance SchemaType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "(pos,e) <- posnElement [s]"
                            $$ text "commit $ do"
                            $$ nest 2
                                  (vcat (zipWith ppAttr as [0..])
                                  $$ text "reparse [CElem e pos]"
                                  $$ text "v <- parseSchemaType s"
                                  $$ text "return $" <+> ppConId nx t
                                                     <+> text "v"
                                                     <+> attrsValue as)
                            )
                  )
    $$ text "instance Extension" <+> ppConId nx t <+> ppConId nx s
                                 <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId nx t <> text " s _) = s")
  where
    t_attrs = let (XName (N t_base)) = t in XName (N (t_base++"Attributes"))

    attrsValue [] = ppConId nx t_attrs
    attrsValue as = parens (ppConId nx t_attrs <+>
                            hsep [text ("a"++show n) | n <- [0..length as-1]])

    -- do element [s]
    --    blah <- attribute foo
    --    interior e $ do
    --        simple <- parseText acceptingParser
    --        return (T simple blah)

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
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "e <- element [s]"
                           $$ text "commit $ interior e $ parseSimpleType")
                  )
    $$ text "instance SimpleType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "acceptingParser ="
                        <+> vcat (intersperse (text "`onFail`")
                                              (map parseItem (i:is))))
  where
    item (i,c) = (ppConId nx t <> text "_" <> ppConId nx i)
                 $$ ppComment After c
    parseItem (i,_) = text "do isWord \"" <> ppXName i <> text "\"; return"
                           <+> (ppConId nx t <> text "_" <> ppConId nx i)

ppHighLevelDecl nx (ElementsAttrs t es as comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "=" <+> ppConId nx t
        $$ nest 8 (ppFields nx t es as)
    $$ text "instance SchemaType" <+> ppConId nx t <+> text "where"
        $$ nest 4 (text "parseSchemaType s = do" 
                  $$ nest 4 (text "(pos,e) <- posnElement [s]"
                            $$ text "commit $ do"
                            $$ nest 2
                                  (vcat (zipWith ppAttr as [0..])
                                  $$ text "interior e $ return"
                                      <+> returnValue as
                                      $$ nest 4 (vcat (map ppElem es))
                                  )
                            )
                  )
  where
    returnValue [] = ppConId nx t
    returnValue as = parens (ppConId nx t <+>
                             hsep [text ("a"++show n) | n <- [0..length as-1]])

ppHighLevelDecl nx (ElementOfType e@Element{}) =
    (text "element" <> ppVarId nx (elem_name e)) <+> text "::"
        <+> text "XMLParser" <+> ppConId nx (elem_type e)
    $$
    (text "element" <> ppVarId nx (elem_name e)) <+> text "="
        <+> (text "parseSchemaType \"" <> ppXName (elem_name e)  <> text "\"")

ppHighLevelDecl nx (Choice t es comm) =
    ppComment Before comm
    $$ text "data" <+> ppConId nx t <+> text "=" <+>
        nest 4 ( choices "=" (head es) 0
               $$ vcat (zipWith (choices "|") (tail es) [1..]) )
  where
    choices c e n = text c <+> (ppConId nx t <> text (show n))
                           <+> ppConId nx (elem_type e)

-- Comment out the Group for now.  Groups get inlined into the ComplexType
-- where they are used, so it may not be sensible to declare them separately
-- as well.
ppHighLevelDecl nx (Group t es comm) = PP.empty
--  ppComment Before comm
--  $$ text "data" <+> ppConId nx t <+> text "="
--                 <+> ppConId nx t <+> hsep (map (ppConId nx . elem_type) es)

-- Possibly we want to declare a really more restrictive type, e.g. 
--    to remove optionality, (Maybe Foo) -> (Foo), [Foo] -> Foo
--    consequently the "restricts" method should do a proper translation,
--    not merely an unwrapping.
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
                                    <+> ppFields nx t es as
    $$ text "instance Extension" <+> ppConId nx t <+> ppConId nx s
                                 <+> ppAuxConId nx t <+> text "where"
        $$ nest 4 (text "supertype (" <> ppConId nx t <> text " s e) = s"
                   $$ text "extension (" <> ppConId nx t <> text " s e) = e")
-}
ppHighLevelDecl nx (ExtendComplexType t s oes oas es as comm) =
    ppHighLevelDecl nx (ElementsAttrs t (oes++es) (oas++as) comm)
    $$ text "instance Extension" <+> ppConId nx t <+> ppConId nx s
                                 <+> text "where"
        $$ nest 4 (text "supertype (" <> ppType t (oes++es) (oas++as)
                                      <> text ") ="
                                      $$ nest 11 (ppType s oes oas) )
  where
    ppType t es as = ppConId nx t
                     <+> hsep (take (length as) [text ('a':show n) | n<-[0..]])
                     <+> hsep (take (length es) [text ('e':show n) | n<-[0..]])

ppHighLevelDecl nx (XSDInclude m comm) =
    ppComment After comm
    $$ text "import" <+> ppModId nx m

ppHighLevelDecl nx (XSDImport m comm) =
    ppComment After comm
    $$ text "import" <+> ppModId nx m

ppHighLevelDecl nx (XSDComment comm) =
    ppComment Before comm


--------------------------------------------------------------------------------

-- | Generate named fields from elements and attributes.
ppFields :: NameConverter -> XName -> [Element] -> [Attribute] -> Doc
ppFields nx t es as | null es && null as = empty
ppFields nx t es as =  vcat ( text "{" <+> head fields
                            : map (text "," <+>) (tail fields)
                            ++ [text "}"] )
  where
    fields = map (ppFieldAttribute nx t) as ++  map (ppFieldElement nx t) es

-- | Generate a single named field from an element.
ppFieldElement :: NameConverter -> XName -> Element -> Int -> Doc
ppFieldElement nx t e@Element{} _ = ppFieldId nx t (elem_name e)
                                        <+> text "::" <+> ppElemTypeName nx id e
                                    $$ ppComment After (elem_comment e)
ppFieldElement nx t e@OneOf{}   i = ppFieldId nx t (XName $ N $"choice"++show i)
                                        <+> text "::" <+> ppElemTypeName nx id e
                                    $$ ppComment After (elem_comment e)

-- | What is the name of the type for an Element (or choice of Elements)?
ppElemTypeName :: NameConverter -> (Doc->Doc) -> Element -> Doc
ppElemTypeName nx brack e@Element{} =
    ppTypeModifier (elem_modifier e) brack $ ppConId nx (elem_type e)
ppElemTypeName nx brack  e@OneOf{}   = 
    brack $ ppTypeModifier (elem_modifier e) parens $
    text "OneOf" <> text (show (length (elem_oneOf e)))
     <+> hsep (map (ppElemTypeName nx parens) (elem_oneOf e))

-- | Generate a single named field from an attribute.
ppFieldAttribute :: NameConverter -> XName -> Attribute -> Doc
ppFieldAttribute nx t a = ppFieldId nx t (attr_name a) <+> text "::"
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

