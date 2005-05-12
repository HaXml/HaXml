module DtdToTypeDefPP
  ( TypeDef
  , dtd2typedef
  , ppTypeDef
  , mkInstance
  , mangle
  ) where

-- This module performs the translation of a parsed XML DTD into a
-- pretty-printed Haskell module of data/newtype definitions.

import Char (isLower, isUpper, toLower, toUpper, isDigit)
#if defined(__HASKELL98__)
import List (intersperse, isPrefixOf)
#else
import List (intersperse)
import IsPrefixOf
#endif
import XmlTypes hiding (Name)
import Pretty


---- Internal representation for typedefs ----
data Name = Name { xName :: String	-- original name
		 , hName :: String	-- & mangled Haskell name
                 }
          deriving Eq

type Constructors = [(Name,[StructType])]
type AttrFields   = [(Name, StructType)]
data TypeDef =
      DataDef Bool Name AttrFields Constructors		-- Bool for main/aux.
    | EnumDef Name [Name]
    deriving Eq
data StructType =
      Maybe StructType
    | Defaultable StructType String	-- String holds default value.
    | List StructType
    | Tuple [StructType]
    | OneOf [StructType]
    | String
    | Defined Name
    deriving Eq


---- Pretty-printing typedefs ----
ppTypeDef :: TypeDef -> Doc

--	no attrs, no constructors
ppTypeDef (DataDef _ n [] []) =
    let name = ppHName n in
    text "data" <+> name <+> text "=" <+> name <+> text "\t\t" <> derives

--	no attrs, single constructor
ppTypeDef (DataDef _ n [] [c@(_,[_])]) =
    text "newtype" <+> ppHName n <+> text "=" <+> ppC c <+> text "\t\t" <> derives

--	no attrs, multiple constrs
ppTypeDef (DataDef _ n [] cs) =
    text "data" <+> ppHName n <+>
           ( text "=" <+> ppC (head cs) $$
             vcat (map (\c-> text "|" <+> ppC c) (tail cs)) $$
             derives )

--	nonzero attrs, no constructors
ppTypeDef (DataDef _ n fs []) =
    let name = ppHName n in
    text "data" <+> name <+> text "=" <+> name $$
    nest 4 ( text "{" <+> ppF (head fs) $$
             vcat (map (\f-> text "," <+> ppF f) (tail fs)) $$
             text "}" <+> derives )

--	nonzero attrs, one or more constrs
ppTypeDef (DataDef _ n fs cs) =
    let attr = ppAName n in
    text "data" <+> ppHName n <+>
           ( text "=" <+> ppAC attr (head cs) $$
             vcat (map (\c-> text "|" <+> ppAC attr c) (tail cs)) $$
             derives )  $$
    text "data" <+> attr <+> text "=" <+> attr $$
    nest 4 ( text "{" <+> ppF (head fs) $$
             vcat (map (\f-> text "," <+> ppF f) (tail fs)) $$
             text "}" <+> derives )

--	enumerations (of attribute values)
ppTypeDef (EnumDef n es) =
    text "data" <+> ppHName n <+>
    ( text "=" <+>
      fsep (intersperse (text " | ") (map ppHName es))
    $$ derives )


ppST :: StructType -> Doc
ppST (Defaultable st _)  = parens (text "Defaultable" <+> ppST st)
ppST (Maybe st)  = parens (text "Maybe" <+> ppST st)
ppST (List st)   = text "[" <> ppST st <> text "]"
ppST (Tuple sts) = parens (commaList (map ppST sts))
ppST (OneOf sts) = parens (text "OneOf" <> text (show (length sts)) <+>
                           hsep (map ppST sts))
ppST  String     = text "String"
ppST (Defined n) = ppHName n

-- constructor and components
ppC :: (Name,[StructType]) -> Doc
ppC (n,sts) = ppHName n <+> fsep (map ppST sts)

-- attribute (fieldname and type)
ppF :: (Name,StructType) -> Doc
ppF (n,st) = ppHName n <+> text "::" <+> ppST st

-- constructor and components with initial attr-type
ppAC :: Doc -> (Name,[StructType]) -> Doc
ppAC atype (n,sts) = ppHName n <+> fsep (atype: map ppST sts)

ppHName, ppXName, ppAName :: Name -> Doc
ppHName (Name _ s) = text s
ppXName (Name s _) = text s
ppAName (Name _ s) = text s <> text "_Attrs"
                      
derives = text "deriving" <+> parens (commaList (map text ["Eq","Show"]))




---- Internal representation for database of DTD decls ----
data Record = R [AttDef] ContentSpec
type Db = [(String,Record)]


---- Build database of DTD decls then convert to typedefs ----
----     (need to merge ELEMENT and ATTLIST decls)
dtd2typedef :: [MarkupDecl] -> [TypeDef]
dtd2typedef mds = 
  (concatMap convert . reverse . database []) mds
  where
  database db [] = db
  database db (m:ms) =
      case m of
        (Element (ElementDecl n cs)) ->
          case lookup n db of
            Nothing -> database ((n, R [] cs):db) ms
            (Just (R as _)) -> database (replace n (R as cs) db) ms
        (AttList (AttListDecl n as)) ->
          case lookup n db of
            Nothing -> database ((n, R as EMPTY):db) ms
            (Just (R _ cs)) -> database (replace n (R as cs) db) ms
    --  (MarkupPE _ m') -> database db (m':ms)
        _ -> database db ms

  replace n v [] = error "dtd2typedef.replace: no element to replace"
  replace n v (x@(n0,_):db)
      | n==n0     = (n,v): db
      | otherwise = x: replace n v db



---- Convert DTD record to typedef ----
convert :: (String, Record) -> [TypeDef]
convert (n, R as cs) =
    case cs of
      EMPTY                   -> modifier None []
      ANY                     -> error "NYI: contentspec of ANY"
      (Mixed PCDATA)          -> modifier None [[String]]
      (Mixed (PCDATAplus ns)) -> modifier Star ([String]: map ((:[]) . Defined . name) ns)
      (ContentSpec cp)        ->
          case cp of
            (TagName n' m) -> modifier m [[Defined (name n')]]
            (Choice cps m) -> modifier m (map inner cps)
            (Seq cps m)    -> modifier m [concatMap inner cps]
    ++ concatMap (mkAttrDef n) as
  where
    attrs    :: AttrFields
    attrs     = map (mkAttrField n) as

    modifier None sts   = mkData sts          attrs False (name n)
    modifier m   [[st]] = mkData [modf m st]  attrs False (name n)
    modifier m    sts   = mkData [modf m (Defined (name_ n))] attrs False (name n) ++
                          mkData sts          []    True  (name_ n)

    inner :: CP -> [StructType]
    inner (TagName n' m) = modf m (Defined (name n'))
    inner (Choice cps m) = modf m (OneOf (concatMap inner cps))
    inner (Seq cps None) = concatMap inner cps
    inner (Seq cps m)    = modf m (Tuple (concatMap inner cps))

    modf None x  = [x]
    modf Query x = [Maybe x]
    modf Star x  = [List x]
    modf Plus x  = [List x]

mkData :: [[StructType]] -> AttrFields -> Bool -> Name -> [TypeDef]
mkData []   fs aux n  = [DataDef aux n fs []]
mkData [ts] fs aux n  = [DataDef aux n fs [(n, ts)]]
mkData tss  fs aux n  = [DataDef aux n fs (map (mkConstr n) tss)]
  where
    mkConstr n ts = (mkConsName n ts, ts)
    mkConsName (Name x n) sts = Name x (n++concat (intersperse "_" (map flatten sts)))
    flatten (Maybe st)   = {-"Maybe_" ++-} flatten st
    flatten (List st)    = {-"List_" ++-} flatten st
    flatten (Tuple sts)  = {-"Tuple" ++ show (length sts) ++ "_" ++-}
                            concat (intersperse "_" (map flatten sts))
    flatten String       = "Str"
    flatten (OneOf sts)  = {-"OneOf" ++ show (length sts) ++ "_" ++-}
                            concat (intersperse "_" (map flatten sts))
    flatten (Defined (Name _ n))  = n

mkAttrDef e (AttDef n StringType def) =
    []
mkAttrDef e (AttDef n (TokenizedType t) def) =
    [] -- mkData [[String]] [] False (name n)
mkAttrDef e (AttDef n (EnumeratedType (NotationType nt)) def) =
    error "NYI: attribute of EnumeratedType/NotationType"
mkAttrDef e (AttDef n (EnumeratedType (Enumeration es)) def) =
    [EnumDef (name_a e n) (map (name_ac e n) es)]
        -- Default attribute values not handled here

mkAttrField :: String -> AttDef -> (Name,StructType)
mkAttrField e (AttDef n typ req) = (name_f e n, mkType typ req)
  where
    mkType StringType REQUIRED = String
    mkType StringType IMPLIED  = Maybe String
    mkType StringType (DefaultTo (AttValue [Left s]) f) = Defaultable String s
    mkType (TokenizedType _) REQUIRED  = String
    mkType (TokenizedType _) IMPLIED   = Maybe String
    mkType (TokenizedType _) (DefaultTo (AttValue [Left s]) f) =
							Defaultable String s
    mkType (EnumeratedType _) REQUIRED = Defined (name_a e n)
    mkType (EnumeratedType _) IMPLIED  = Maybe (Defined (name_a e n))
    mkType (EnumeratedType _) (DefaultTo (AttValue [Left s]) f) =
		Defaultable (Defined (name_a e n)) (hName (name_ac e n s))

name n     = Name n (mangle n)
name_ n    = Name n (mangle n ++ "_")
name_a e n = Name n (mangle e ++ "_" ++ mangle n)
		-- prefix enumeration type in an attribute with element-tag name
name_ac e t n = Name n (mangle e ++ "_" ++ mangle t ++ "_" ++ mangle n)
		-- prefix enumeration constructor with element-tag name
name_f e n = Name n (manglef e ++ mangle n)	-- prefix fieldname with element
name_at n  = Name n (mangle n ++ "_Attrs")	-- obsolete

mangle (n:ns)
    | isLower n   = toUpper n: map decolonify ns
    | isDigit n   = 'I': n: map decolonify ns
    | otherwise   = n: map decolonify ns
manglef (n:ns)
    | isUpper n   = toLower n: map decolonify ns
    | isDigit n   = '_': n: map decolonify ns
    | otherwise   = n: map decolonify ns
decolonify ':' = '\''	-- TODO: turn namespaces into qualified identifiers
decolonify '-' = '_'
decolonify  c  = c



---- Convert typedef to appropriate instance declaration for I/O ----
mkInstance :: TypeDef -> Doc

-- no constructors
mkInstance (DataDef aux n fs []) =
    let (frpat, frattr, topat, toattr) = attrpats fs
        frretval = if null fs then ppHName n else frattr
        topatval = if null fs then ppHName n else topat
    in
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( text "fromElem (CElem (Elem \"" <> ppXName n <> text "\""
                  <+> frpat <+> text "[]):rest) =" $$
             nest 4 (text "(Just" <+> frretval <> text ", rest)") $$
             text "fromElem rest = (Nothing, rest)"
           $$
             text "toElem" <+> topatval <+> text "=" $$
             nest 4 (text "[CElem (Elem \"" <> ppXName n <> text "\""
                          <+> toattr <+> text "[])]")
           )
    $$
    mkInstanceAttrs Same n fs

-- single constructor
mkInstance (DataDef aux n fs [(n0,sts)]) =
    let vs = nameSupply sts
        (frpat, frattr, topat, toattr) = attrpats fs
    in
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( text "fromElem (CElem (Elem \"" <> ppXName n <> text "\""
                  <+> frpat <+> text "c0):rest) =" $$
             nest 4 (mkFrElem n sts vs (
                         text "(Just" <+> parens (mkCpat n0 frattr vs)
                               <> text ", rest)")
                    ) $$
             text "fromElem rest = (Nothing, rest)"
           $$
             text "toElem" <+> parens (mkCpat n0 topat vs) <+> text "=" $$
             nest 4 (text "[CElem (Elem \"" <> ppXName n <> text "\""
                          <+> toattr <+> parens (mkToElem sts vs) <> text ")]")
           )
    $$
    mkInstanceAttrs Extended n fs

-- multiple constructors
mkInstance (DataDef aux n fs cs) =
    let vs = nameSupply cs
        (frpat, frattr, topat, toattr) = attrpats fs
        mixattrs = if null fs then False else True
    in
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( ( if aux then text "fromElem c0 ="
               else text "fromElem (CElem (Elem \"" <> ppXName n <> text "\""
                         <+> frpat <+> text "c0):rest) =" ) $$
             mkFrAux aux frattr cs $$
             text "fromElem rest = (Nothing, rest)"
           $$
             if aux then vcat (map (mkToAux mixattrs) cs)
             else vcat (map (mkToMult n topat toattr) cs)
           )
    $$
    mkInstanceAttrs Extended n fs

-- enumeration of attribute values
mkInstance (EnumDef n es) =
    text "instance XmlAttrType" <+> ppHName n <+> text "where" $$
    nest 4 ( text "fromAttrToTyp n (n',v)" $$
             nest 4 (text "| n==n'     = translate (attr2str v)" $$
                     text "| otherwise = Nothing") $$
             nest 2 (text "where" <+> mkTranslate es)
           $$
             vcat (map mkToAttr es)
           )


data SameName = Same | Extended

mkInstanceAttrs        :: SameName -> Name -> AttrFields -> Doc
mkInstanceAttrs s n []  = empty
mkInstanceAttrs s n fs  =
    let ppName = case s of { Same-> ppHName;  Extended-> ppAName; }
    in
    text "instance XmlAttributes" <+> ppName n <+> text "where" $$
    nest 4 ( text "fromAttrs as =" $$
             nest 4 ( ppName n $$
                      nest 2 (vcat ((text "{" <+> mkFrFld n (head fs)):
                                     map (\x-> comma <+> mkFrFld n x) (tail fs)) $$
                              text "}"))
           $$
             text "toAttrs v = catMaybes " $$
             nest 4 (vcat ((text "[" <+> mkToFld (head fs)):
                           map (\x-> comma <+> mkToFld x) (tail fs)) $$
                     text "]")
           )


--                  respectively (frpat,frattr,topat,toattr)
attrpats :: AttrFields -> (Doc,Doc,Doc,Doc)
attrpats fs =
  if null fs then (text "[]", empty, empty, text "[]")
  else (text "as", parens (text "fromAttrs as"), text "as", parens (text "toAttrs as"))




mkFrElem :: Name -> [StructType] -> [Doc] -> Doc -> Doc
mkFrElem n sts vs inner =
    foldr (frElem n) inner (zip3 sts vs cvs)
  where
    cvs = let ns = nameSupply2 vs
          in zip ns (text "c0": init ns)
    frElem n (st,v,(cvi,cvo)) inner =
        parens (text "\\" <> parens (v<>comma<>cvi) <> text "->" $$
                nest 2 inner) $$
        parens (
          case st of
            (Maybe String)  -> text "fromText" <+> cvo
            (Maybe s)       -> text "fromElem" <+> cvo
            (List String)   -> text "many fromText" <+> cvo
            (List s)        -> text "many fromElem" <+> cvo
            (Tuple ss)  -> text "nyi_fromElem_Tuple" <+> cvo
            (OneOf ss)  -> text "fromElem" <+> cvo
            (String)    -> text "definite fromText" <+> text "\"text\" \"" <>
                                                 ppXName n <> text "\"" <+> cvo
            (Defined m) -> text "definite fromElem" <+>
				 text "\"<" <> ppXName m <> text ">\" \"" <>
                                                 ppXName n <> text "\"" <+> cvo
            (Defaultable _ _)  -> text "nyi_fromElem_Defaultable" <+> cvo
          )

mkToElem :: [StructType] -> [Doc] -> Doc
mkToElem []  [] = text "[]"
mkToElem sts vs =
    fsep (intersperse (text "++") (zipWith toElem sts vs))
  where
    toElem st v =
      case st of
        (Maybe String)  -> text "maybe [] toText" <+> v
        (Maybe s)       -> text "maybe [] toElem" <+> v
        (List String)   -> text "concatMap toText" <+> v
        (List s)        -> text "concatMap toElem" <+> v
        (Tuple ss)      -> text "toElem" <+> v
        (OneOf ss)      -> text "toElem" <+> v
        (String)        -> text "toText" <+> v
        (Defined m)     -> text "toElem" <+> v
        (Defaultable _ _)  -> text "nyi_toElem_Defaultable" <+> v

mkRpat :: [Doc] -> Doc
mkRpat [v] = v
mkRpat vs  = (parens . hcat . intersperse comma) vs

mkCpat :: Name -> Doc -> [Doc] -> Doc
mkCpat n i vs = ppHName n <+> i <+> fsep vs

nameSupply,nameSupply2 :: [b] -> [Doc]
nameSupply  ss = take (length ss) (map char ['a'..])
nameSupply2 ss = take (length ss) [ text ('c':v:[]) | v <- ['a'..]]

mkTranslate :: [Name] -> Doc
mkTranslate es =
    vcat (map trans es) $$
    text "translate _ = Nothing"
  where
    trans n = text "translate \"" <> ppXName n <> text "\" =" <+>
              text "Just" <+> ppHName n

mkToAttr n = text "toAttrFrTyp n" <+> ppHName n <+> text "=" <+>
             text "Just (n, str2attr" <+> doubleQuotes (ppXName n) <> text ")"

mkFrFld :: Name -> (Name,StructType) -> Doc
mkFrFld tag (n,st) =
    ppHName n <+> text "=" <+>
    ( case st of
        (Defaultable String s) -> text "defaultA fromAttrToStr" <+>
						 doubleQuotes (text s)
        (Defaultable _ s)      -> text "defaultA fromAttrToTyp" <+> text s
        (Maybe String)         -> text "possibleA fromAttrToStr"
        (Maybe _)              -> text "possibleA fromAttrToTyp"
        String                 -> text "definiteA fromAttrToStr" <+>
						 doubleQuotes (ppXName tag)
        _                      -> text "definiteA fromAttrToTyp" <+>
						 doubleQuotes (ppXName tag)
    ) <+> doubleQuotes (ppXName n) <+> text "as"

mkToFld :: (Name,StructType) -> Doc
mkToFld (n,st) =
    ( case st of
        (Defaultable String _) -> text "defaultToAttr toAttrFrStr"
        (Defaultable _ _)      -> text "defaultToAttr toAttrFrTyp"
        (Maybe String)         -> text "maybeToAttr toAttrFrStr"
        (Maybe _)              -> text "maybeToAttr toAttrFrTyp"
        String                 -> text "toAttrFrStr"
        _                      -> text "toAttrFrTyp"
    ) <+> doubleQuotes (ppXName n) <+> parens (ppHName n <+> text "v")

mkFrAux :: Bool -> Doc -> [(Name,[StructType])] -> Doc
mkFrAux keeprest attrs cs = foldr frAux inner cs
  where
    inner = text "(Nothing, c0)"
    rest  = if keeprest then text "rest" else text "_"
    frAux (n,sts) inner =
        let vs  = nameSupply sts in
        nest 4 (text "case" <+> blah sts vs <+> text "of" $$
                succpat sts vs <+> text "-> (Just" <+>
                                   parens (mkCpat n attrs vs) <> text ", rest)"
                $$
                failpat sts <+> text "->" $$ nest 4 inner
               )
    blah [st] [v] =
        blahblahblah st (text "c0")
    blah sts vs =
        let ns = nameSupply2 vs
            cvs = zip ns (text "c0": init ns)
            blahblah (st,v,(cvi,cvo)) inner =
                parens (text "\\" <> parens (v<>comma<>cvi) <> text "->" $$
                        nest 2 inner) $$
                blahblahblah st cvo
        in
        foldr blahblah (mkRpat (vs++[last ns])) (zip3 sts vs cvs)
    blahblahblah st cvo = parens (
        case st of
          (Maybe String) -> text "fromText" <+> cvo
          (Maybe s)      -> text "fromElem" <+> cvo
          (List String)  -> text "many fromText" <+> cvo
          (List s)       -> text "many fromElem" <+> cvo
          (Tuple ss)     -> text "nyi_fromText_Tuple"
          (OneOf ss)     -> text "fromElem" <+> cvo
          (String)       -> text "fromText" <+> cvo
          (Defined m)    -> text "fromElem" <+> cvo
        )
    failpat sts =
        let fp st =
                case st of
                  (Maybe s)   -> text "Nothing"
                  (List s)    -> text "[]"
                  (Tuple ss)  -> text "nyi_failpat_Tuple"
                  (OneOf ss)  -> text "Nothing"
                  (String)    -> text "Nothing"
                  (Defined m) -> text "Nothing"
        in parens (hcat (intersperse comma (map fp sts++[text "_"])))
    succpat sts vs =
        let sp st v =
                case st of
                  (Maybe s)   -> v
                  (List s)    -> v
                  (Tuple ss)  -> text "nyi_succpat_Tuple"
                  (OneOf ss)  -> text "Just" <+> v
                  (String)    -> text "Just" <+> v
                  (Defined m) -> text "Just" <+> v
        in parens (hcat (intersperse comma (zipWith sp sts vs++[rest])))

mkToAux :: Bool -> (Name,[StructType]) -> Doc
mkToAux mixattrs (n,sts) =
    let vs = nameSupply sts
        attrs = if mixattrs then text "as" else empty
    in
    text "toElem" <+> parens (mkCpat n attrs vs) <+> text "=" <+>
    mkToElem sts vs

mkToMult :: Name -> Doc -> Doc -> (Name,[StructType]) -> Doc
mkToMult tag attrpat attrexp (n,sts) =
    let vs = nameSupply sts
    in
    text "toElem" <+> parens (mkCpat n attrpat vs) <+> text "=" <+>
    text "[CElem (Elem \"" <> ppXName tag <> text "\""<+> attrexp <+>
    parens (mkToElem sts vs) <+> text ")]"

