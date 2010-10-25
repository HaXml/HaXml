-- | A type model for Haskell datatypes that bears a reasonable correspondence
--   to the XSD type model.
module Text.XML.HaXml.Schema.NameConversion
  ( module Text.XML.HaXml.Schema.NameConversion
  ) where

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces

import Char
import List

-- | An XName just holds the original XSD qualified name.  It does not
--   ensure that the string conforms to any rules of the various Haskell
--   namespaces.  Use a NameConverter to define how you would like names
--   to be mangled.
newtype XName = XName QName
  deriving (Eq,Show)

-- | An HName is a resolved version of an XName.  It should conform to
--   the various namespace rules, and may already include a module
--   qualifier if appropriate.
newtype HName = HName String
    deriving Show

-- | A NameConverter is a collection of functions that convert an XName
--   into an HName, for various Haskell namespaces.  You can define your
--   own arbitrary resolver, but should ensure that you abide by the
--   Haskell rules for conid, varid, etc.
data NameConverter = NameConverter
                       { modid    :: XName -> HName
                       , conid    :: XName -> HName
                       , varid    :: XName -> HName
                       , unqconid :: XName -> HName
                       , unqvarid :: XName -> HName
                       , fieldid  :: XName -> XName -> HName
                       }

-- | A simple default set of rules for resolving XNames into HNames.
simpleNameConverter :: NameConverter
simpleNameConverter = NameConverter
    { modid    = \(XName qn)-> HName . mkConid . hierarchy $ qn
    , conid    = \(XName qn)-> HName . mkConid . hierarchy $ qn
    , varid    = \(XName qn)-> HName . mkVarid . last avoidKeywords
                                               . hierarchy $ qn
    , unqconid = \(XName qn)-> HName . mkConid . local $ qn
    , unqvarid = \(XName qn)-> HName . mkVarid . last avoidKeywords
                                               . local $ qn
    , fieldid  = \(XName qnt) (XName qnf)->
                               HName $ (mkVarid . last id . hierarchy $ qnt)
                                       ++ "_" ++
                                       (mkVarid . last id . hierarchy $ qnf)
    }
  where
    hierarchy (N n)     = wordsBy (==':') n
    hierarchy (QN ns n) = [nsPrefix ns, n]

    local               = (:[]) . Prelude.last . hierarchy

    mkConid  [c]        | map toLower c == "string"     = "Xsd.XsdString"
                        | otherwise = first toUpper c
    mkConid [m,c]       | map toLower c == "string"     = "Xsd.XsdString"
                        | otherwise = first toUpper m++"."++first toUpper c
    mkVarid  [v]        = first toLower v
    mkVarid [m,v]       = first toUpper m++"."++first toLower v

    first f (x:xs)
      | not (isAlpha x) = f 'v': x: xs
      | otherwise       = f x: xs
    last  f [x]         = [ f x ]
    last  f (x:xs)      = x: last f xs

 -- cleanUp = map (\c-> if not (isAlphaNum c) then '_' else c)

-- | Ensure that a string does not match a Haskell keyword.
avoidKeywords :: String -> String
avoidKeywords s
    | s `elem` keywords  = s++"_"
    | otherwise          = s
  where
    keywords = [ "case", "of", "data", "default", "deriving", "do"
               , "forall", "foreign", "if", "then", "else", "import"
               , "infix", "infixl", "infixr", "instance", "let", "in"
               , "module", "newtype", "qualified", "type", "where" ]


-- | A specialised module-name converter for FpML module names with
--   multiple dashes, including version numbers,
--   e.g. fpml-dividend-swaps-4-7.xsd      becomes FpML.V47.Swaps.Dividend
--   but  fpml-posttrade-execution-4-7.xsd becomes FpML.V47.PostTrade.Execution
fpml :: String -> String
fpml = concat
         . intersperse "."    -- put the dots in
         . ("Data":)          -- root of the Haskell module namespace
         . rearrange          -- hierarchy shuffling, dependent on names
         . map cap            -- make into nice module names
         . version            -- move version number to front
         . wordsBy (=='-')    -- separate words
         . basename ".xsd"    -- strip .xsd if present
  where
    version ws = let (last2,remain) = splitAt 2 . reverse $ ws in
                 if all (all isDigit) last2 && length ws > 2
                 then head ws: ('V':concat (reverse last2))
                             : tail (reverse remain)
                 else ws
    rearrange [a,v,"PostTrade",c] = [a,v,"PostTrade",c]
    rearrange [a,v,b,c]           = [a,v,c,b]
    rearrange [a,v,b,c,d]         = [a,v,d,b++c]
    rearrange [a,v,b,c,d,e]       = [a,v,e,b++c++d]
    rearrange v                   = v

    cap :: String -> String
    cap "Fpml"      = "FpML"
    cap "fpml"      = "FpML"
    cap "cd"        = "CD"
    cap "eq"        = "EQ"
    cap "fx"        = "FX"
    cap "ird"       = "IRD"
    cap "posttrade" = "PostTrade"
    cap "pretrade"  = "PreTrade"
    cap (c:cs)      = toUpper c: cs


-- | Chop a list into segments, at separators identified by the predicate.
--   The separator items are discarded.
wordsBy :: (a->Bool) -> [a] -> [[a]]
wordsBy pred = wordsBy' pred []
  where wordsBy' p []  []     = []
        wordsBy' p acc []     = [reverse acc]
        wordsBy' p acc (c:cs) | p c       = reverse acc :
                                            wordsBy' p [] (dropWhile p cs)
                              | otherwise = wordsBy' p (c:acc) cs

-- | Remove any prefix directory names, and given suffix extension.
basename :: String -> String -> String
basename ext = reverse . snip (reverse ext)
                       . takeWhile (not.(`elem`"\\/")) . reverse
    where snip p s = if p `isPrefixOf`s then drop (length p) s else s

fpmlNameConverter :: NameConverter
fpmlNameConverter = simpleNameConverter
    { modid   = (\(HName h)-> HName (fpml h))
                . modid simpleNameConverter
 -- , conid   = (\(HName h)-> case take 4 (reverse h) of
 --                             "munE" -> HName (reverse (drop 4 (reverse h)))
 --                             _      -> HName h )
 --             . conid simpleNameConverter
    , fieldid  = \(XName qnt) (XName qnf)->
                  let t = mkVarId . last . hierarchy $ qnt
                      f = mkVarId . last . hierarchy $ qnf
                  in HName $ if t==f then f
                             else if t `isPrefixOf` f
                             then t ++"_"++ mkVarId (drop (length t) f)
                             else mkVarId (shorten (mkConId t)) ++"_"++ f
    }
  where
    hierarchy (N n)     = wordsBy (==':') n
    hierarchy (QN ns n) = [nsPrefix ns, n]

    mkVarId   ("id")    = "ID"
    mkVarId   (v:vs)    = toLower v: vs
    mkConId   (v:vs)    = toUpper v: vs

    shorten t | length t <= 12 = t
              | length t <  35 = concatMap shortenWord (splitWords t)
              | otherwise      = map toLower (head t: filter isUpper (tail t))
    splitWords "" = []
    splitWords (u:s)  = let (w,rest) = span (not . isUpper) s
                        in (u:w) : splitWords rest

    shortenWord "Request"     = "Req" -- some special cases
    shortenWord "Reference"   = "Ref"
    shortenWord "Valuation"   = "Val"
    shortenWord "Calendar"    = "Cal"
    shortenWord "Absolute"    = "Abs"
    shortenWord "Additional"  = "Add"
    shortenWord "Business"    = "Bus"
    shortenWord "Standard"    = "Std"
    shortenWord "Calculation" = "Calc"
    shortenWord "Quotation"   = "Quot"
    shortenWord "Information" = "Info"
    shortenWord "Exchange"    = "Exch"
    shortenWord "Characteristics" = "Char"
    shortenWord "Multiple"    = "Multi"
    shortenWord "Constituent" = "Constit"
    shortenWord "Convertible" = "Convert"
    shortenWord "Underlyer"   = "Underly"
    shortenWord "Underlying"  = "Underly"
    shortenWord "Properties"  = "Props"
    shortenWord "Property"    = "Prop"
    shortenWord w | length w < 8 = w   -- then the general rule
                  | otherwise    = case splitAt 5 w of
                                     (pref,c:suf) | isVowel c -> pref
                                                  | otherwise -> pref++[c]

    isVowel = (`elem` "aeiouy")

