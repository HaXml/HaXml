-- | Validate a document against a dtd.
module Text.Xml.HaXml.Validate
  ( validate
  ) where

import Text.Xml.HaXml.Types
import Text.Xml.HaXml.Xml2Haskell (attr2str)
import Maybe (fromMaybe,isNothing,fromJust)
import List (intersperse)

#ifdef __GLASGOW_HASKELL__
-- real finite map, if it is available
import FiniteMap
#else
-- otherwise, a very simple and inefficient implementation of a finite map
type FiniteMap a b = [(a,b)]
listToFM :: [(a,b)] -> FiniteMap a b
listToFM = id
lookupFM :: FiniteMap a b -> a -> Maybe b
lookupFM fm k = lookup k fm
#endif

-- gather appropriate information out of the DTD
data SimpleDTD = SimpleDTD
    { elements   :: FiniteMap Name ContentSpec
    , attributes :: FiniteMap (Name,Name) AttType
    , required   :: FiniteMap Name [Name]	-- required attributes
    }

simplifyDTD :: DocTypeDecl -> SimpleDTD
simplifyDTD (DTD _ _ decls) =
    SimpleDTD
      { elements   = listToFM [ (name,content)
                              | Element (ElementDecl name content) <- decls ]
      , attributes = listToFM [ ((elem,attr),typ)
                              | AttList (AttListDecl elem attdefs) <- decls
                              , AttDef attr typ _ <- attdefs ]
      , required   = listToFM [ (elem, [ attr
                                       | AttDef attr _ REQUIRED <- attdefs ])
                              | AttList (AttListDecl elem attdefs) <- decls ]
      }

-- simple auxiliary to avoid lots of if-then-else with empty else clauses.
gives :: Bool -> a -> [a]
True `gives` x = [x]
False `gives` _ = []

-- | 'validate' takes a DTD and a tagged element, and returns a list of
--   errors in the document with respect to its DTD.
--
--   If you have several documents to validate against a single DTD,
--   then you will gain efficiency by `freezing-in' the DTD by partial
--   application, e.g. "checkMyDTD = validate myDTD".
validate :: DocTypeDecl -> Element -> [String]
validate dtd' elem = valid elem
  where
    dtd = simplifyDTD dtd'

    valid (Elem name attrs contents) =
        let spec = lookupFM (elements dtd) name in 
        (isNothing spec) `gives` ("Element <"++name++"> not known.")
        ++ concatMap (checkAttr name) attrs
        ++ concatMap (checkRequired name attrs)
                     (fromMaybe [] (lookupFM (required dtd) name))
        ++ checkContentSpec name (fromMaybe ANY spec) contents
        ++ concatMap valid [ elem | CElem elem <- contents ]

    checkAttr elem (attr, val) =
        let typ = lookupFM (attributes dtd) (elem,attr)
            attval = attr2str val in
        if isNothing typ then ["Attribute \""++attr
                               ++"\" not known for element <"++elem++">."]
        else
          case fromJust typ of
            EnumeratedType e ->
              case e of
                Enumeration es ->
                    (not (attval `Prelude.elem` es)) `gives`
                          ("Value \""++attval++"\" of attribute \""
                           ++attr++"\" in element <"++elem
                           ++"> is not in the required enumeration range: "
                           ++unwords es)
                _ -> []
            _ -> []

    checkRequired elem attrs req =
        (not (req `Prelude.elem` map fst attrs)) `gives`
            ("Element <"++elem++"> requires the attribute \""++req
             ++"\" but it is missing.")

    checkContentSpec elem ANY _ = []
    checkContentSpec elem EMPTY [] = []
    checkContentSpec elem EMPTY (_:_) =
        ["Element <"++elem++"> is not empty but should be."]
    checkContentSpec elem (Mixed PCDATA) cs = concatMap (checkMixed elem []) cs
    checkContentSpec elem (Mixed (PCDATAplus names)) cs =
        concatMap (checkMixed elem names) cs
    checkContentSpec elem (ContentSpec cp) cs = excludeText elem cs ++
        (let (errs,rest) = checkCP elem cp (flatten cs) in
         case rest of [] -> errs
                      _  -> errs++["Element <"++elem++"> contains extra "
                                  ++"elements beyond its content spec."])

    checkMixed elem permitted (CElem (Elem name _ _))
        | not (name `Prelude.elem` permitted) =
            ["Element <"++elem++"> contains an element <"++name
             ++"> but should not."]
    checkMixed elem permitted _ = []

    flatten (CElem (Elem name _ _): cs) = name: flatten cs
    flatten (_: cs)                     = flatten cs
    flatten []                          = []

    excludeText elem (CElem _: cs) = excludeText elem cs
    excludeText elem (CMisc _: cs) = excludeText elem cs
    excludeText elem (_:  cs) =
        ["Element <"++elem++"> contains text/references but should not."]
    excludeText elem [] = []

    -- This is a little parser really.  Returns any errors, plus the remainder
    -- of the input string.
    checkCP :: Name -> CP -> [Name] -> ([String],[Name])
    checkCP elem cp@(TagName n None) [] = (cpError elem cp, [])
    checkCP elem cp@(TagName n None) (n':ns)
        | n==n'     = ([], ns)
        | otherwise = (cpError elem cp, n':ns)
    checkCP elem cp@(TagName n Query) [] = ([],[])
    checkCP elem cp@(TagName n Query) (n':ns)
        | n==n'     = ([], ns)
        | otherwise = ([], n':ns)
    checkCP elem cp@(TagName n Star) [] = ([],[])
    checkCP elem cp@(TagName n Star) (n':ns)
        | n==n'     = checkCP elem (TagName n Star) ns
        | otherwise = ([], n':ns)
    checkCP elem cp@(TagName n Plus) [] = (cpError elem cp, [])
    checkCP elem cp@(TagName n Plus) (n':ns)
        | n==n'     = checkCP elem (TagName n Star) ns
        | otherwise = (cpError elem cp, n':ns)
    checkCP elem cp@(Choice cps None) [] = (cpError elem cp, [])
    checkCP elem cp@(Choice cps None) ns =
        let next = choice elem ns cps in
        if null next then (cpError elem cp, ns)
        else ([], head next)	-- choose the first alternative with no errors
    checkCP elem cp@(Choice cps Query) [] = ([],[])
    checkCP elem cp@(Choice cps Query) ns =
        let next = choice elem ns cps in
        if null next then ([],ns)
        else ([], head next)
    checkCP elem cp@(Choice cps Star) [] = ([],[])
    checkCP elem cp@(Choice cps Star) ns =
        let next = choice elem ns cps in
        if null next then ([],ns)
        else checkCP elem (Choice cps Star) (head next)
    checkCP elem cp@(Choice cps Plus) [] = (cpError elem cp, [])
    checkCP elem cp@(Choice cps Plus) ns =
        let next = choice elem ns cps in
        if null next then (cpError elem cp, ns)
        else checkCP elem (Choice cps Star) (head next)
    checkCP elem cp@(Seq cps None) [] = (cpError elem cp, [])
    checkCP elem cp@(Seq cps None) ns =
        let (errs,next) = sequence elem ns cps in
        if null errs then ([],next)
        else (cpError elem cp++errs, ns)
    checkCP elem cp@(Seq cps Query) [] = ([],[])
    checkCP elem cp@(Seq cps Query) ns =
        let (errs,next) = sequence elem ns cps in
        if null errs then ([],next)
        else ([], ns)
    checkCP elem cp@(Seq cps Star) [] = ([],[])
    checkCP elem cp@(Seq cps Star) ns =
        let (errs,next) = sequence elem ns cps in
        if null errs then checkCP elem (Seq cps Star) next
        else ([], ns)
    checkCP elem cp@(Seq cps Plus) [] = (cpError elem cp, [])
    checkCP elem cp@(Seq cps Plus) ns =
        let (errs,next) = sequence elem ns cps in
        if null errs then checkCP elem (Seq cps Star) next
        else (cpError elem cp++errs, ns)

    choice elem ns cps =  -- return only those parses that don't give any errors
        [ rem | ([],rem) <- map (\cp-> checkCP elem cp ns) cps ]
    sequence elem ns cps =  -- accumulate errors down the sequence
        foldl (\(es,ns) cp-> let (es',ns') = checkCP elem cp ns
                             in (es++es', ns'))
              ([],ns) cps


cpError :: Name -> CP -> [String]
cpError elem cp =
    ["Element <"++elem++"> should contain "++display cp++" but does not."]


display :: CP -> String
display (TagName name mod) = name ++ modifier mod
display (Choice cps mod)   = "(" ++ concat (intersperse "|" (map display cps))
                             ++ ")" ++ modifier mod
display (Seq cps mod)      = "(" ++ concat (intersperse "," (map display cps))
                             ++ ")" ++ modifier mod

modifier :: Modifier -> String
modifier None  = ""
modifier Query = "?"
modifier Star  = "*"
modifier Plus  = "+"
