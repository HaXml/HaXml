-- | These are just some common abbreviations for generating HTML
--   content within the XML transformation framework defined
--   by "Text.Xml.HaXml.Combinators".
module Text.XML.HaXml.Html.Generate
  ( -- * HTML construction filters
  -- ** Containers
    html
  , hhead
  , htitle
  , hbody
  , h1, h2, h3, h4
  , hpara
  , hdiv, hspan, margin
  -- ** Anchors
  , anchor, makehref, anchorname
  -- ** Text style
  , hpre
  , hcentre
  , hem, htt, hbold
  , parens, bullet
  -- ** Tables
  , htable, hrow, hcol
  -- ** Breaks, lines
  , hbr, hhr
  -- ** Attributes
  , showattr, (!), (?)
  -- * A simple HTML pretty-printer
  , htmlprint
  ) where

import Char (isSpace)
import List (partition)

import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import qualified Text.PrettyPrint.HughesPJ as Pretty

---- Constructor functions

html, hhead, htitle, hbody, h1, h2, h3, h4, hpara, hpre, hcentre,
    hem, htt, hbold, htable, hrow, hcol, hdiv, hspan, margin
       :: [CFilter] -> CFilter
html    = mkElem "HTML"
hhead   = mkElem "HEAD"
htitle  = mkElem "TITLE"
hbody   = mkElem "BODY"
h1      = mkElem "H1"
h2      = mkElem "H2"
h3      = mkElem "H3"
h4      = mkElem "H4"
hpara   = mkElem "P"
hpre    = mkElem "PRE"
hcentre = mkElem "CENTER"
hem     = mkElem "EM"
htt     = mkElem "TT"
hbold   = mkElem "B"

htable = mkElem "TABLE"
hrow   = mkElem "TR"
hcol   = mkElem "TD"

hdiv   = mkElem "DIV"
hspan  = mkElem "SPAN"
margin = mkElemAttr "DIV" [("margin-left",("2em"!)),
                           ("margin-top", ("1em"!))]

anchor      :: [(String, CFilter)] -> [CFilter] -> CFilter
anchor       = mkElemAttr "A"

makehref, anchorname :: CFilter -> [CFilter] -> CFilter
makehref r   = anchor [ ("href",r) ]
anchorname n = anchor [ ("name",n) ]


hbr, hhr :: CFilter
hbr       = mkElem "BR" []
hhr       = mkElem "HR" []


showattr, (!), (?) :: String -> CFilter
showattr n = find n literal
(!) = literal
(?) = showattr

parens :: CFilter -> CFilter
parens f = cat [ literal "(", f, literal ")" ]

bullet :: [CFilter] -> CFilter
bullet = cat . (literal "M-^U":)


---- Printing function

-- htmlprint :: [Content] -> String
-- htmlprint = concatMap cprint
--   where
--   cprint (CElem e _) = elem e 
--   cprint (CString _ s) = s
--   cprint (CMisc m) = ""
--  
--   elem (Elem n as []) = "\n<"++n++attrs as++" />"
--   elem (Elem n as cs) = "\n<"++n++attrs as++">"++htmlprint cs++"\n</"++n++">"
-- 
--   attrs = concatMap attr
--   attr (n,v) = " "++n++"='"++v++"'"


htmlprint :: [Content] -> Pretty.Doc
htmlprint = Pretty.cat . map cprint . foldrefs 
  where
  foldrefs [] = []
  foldrefs (CString ws s1:CRef r:CString _ s2:cs) =
              CString ws (s1++"&"++ref r++";"++s2): foldrefs cs
  foldrefs (c:cs) = c : foldrefs cs

--ref (RefEntity (EntityRef n)) = n	-- Actually, should look-up symtable.
--ref (RefChar (CharRef s)) = s
  ref (RefEntity n) = n	-- Actually, should look-up symtable.
  ref (RefChar s) = s

  cprint (CElem e)      = elem e
  cprint (CString ws s) = Pretty.cat (map Pretty.text (fmt 60
                                             ((if ws then id else deSpace) s)))
  cprint (CRef r)       = Pretty.text ("&"++ref r++";")
  cprint (CMisc m)      = Pretty.empty
 
  elem (Elem n as []) = Pretty.text "<"   Pretty.<>
                        Pretty.text n     Pretty.<>
                        attrs as          Pretty.<>
                        Pretty.text " />"
  elem (Elem n as cs) =
                    --  ( Pretty.text "<"   Pretty.<>
                    --    Pretty.text n     Pretty.<>
                    --    attrs as          Pretty.<>
                    --    Pretty.text ">")  Pretty.$$
                    --  Pretty.nest 6 (htmlprint cs)  Pretty.$$
                    --  ( Pretty.text "</"  Pretty.<>
                    --    Pretty.text n     Pretty.<>
                    --    Pretty.text ">" )
                        Pretty.fcat [ ( Pretty.text "<"   Pretty.<>
                                        Pretty.text n     Pretty.<>
                                        attrs as          Pretty.<>
                                        Pretty.text ">")
                                    , Pretty.nest 4 (htmlprint cs)
                                    , ( Pretty.text "</"  Pretty.<>
                                        Pretty.text n     Pretty.<>
                                        Pretty.text ">" )
                                    ]

  attrs = Pretty.cat . map attr
  attr (n,AttValue [Left v]) =
               Pretty.text " "  Pretty.<>
               Pretty.text n    Pretty.<>
               Pretty.text "='" Pretty.<>
               Pretty.text v    Pretty.<>
               Pretty.text "'"

  fmt n [] = []
  fmt n s  = let (top,bot) = splitAt n s
                 (word,left) = keepUntil isSpace (reverse top)
             in if length top < n then [s]
                else if not (null left) then
                     reverse left: fmt n (word++bot)
                else let (big,rest) = keepUntil isSpace s
                     in reverse big: fmt n rest

  deSpace []     = []
  deSpace (c:cs) | c=='\n'   = deSpace (' ':cs)
                 | isSpace c = c : deSpace (dropWhile isSpace cs)
                 | otherwise = c : deSpace cs

  keepUntil p xs = select p ([],xs)
      where select p (ls,[])     = (ls,[])
            select p (ls,(x:xs)) | p x       = (ls,x:xs)
                                 | otherwise = select p (x:ls,xs)
