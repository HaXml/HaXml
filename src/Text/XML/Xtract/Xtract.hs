module Main where
------------------------------------------------------------
-- The Xtract tool - an XML-grep.
------------------------------------------------------------ 
import System (getArgs, exitWith, ExitCode(..))
import IO
import Char         (toUpper)
import List         (isSuffixOf)

import Text.Xml.HaXml.Types
import Text.Xml.HaXml.Parse      (xmlParse)
import Text.Xml.HaXml.HtmlParse  (htmlParse)
import Text.Xml.Xtract.Parse     (parseXtract)
import Text.PrettyPrint.HughesPJ (render, vcat)
import Text.Xml.HaXml.PP         (content)
import Text.Xml.HaXml.HtmlGen    (htmlprint)


main =
  getArgs >>= \args->
  if length args < 1 then
    putStrLn "Usage: Xtract <pattern> [xmlfile ...]" >>
    exitWith (ExitFailure 1)
  else
    let (pattern:files) = args
--      findcontents =
--        if null files then (getContents >>= \x-> return [xmlParse "<stdin>"x])
--        else mapM (\x-> do c <- (if x=="-" then getContents else readFile x)
--                           return ((if isHTML x
--                                    then htmlParse x else xmlParse x) c))
--                  files
        xmlSelection  = parseXtract pattern
        htmlSelection = parseXtract (map toUpper pattern)
    in
--  findcontents >>= \cs->
--  ( hPutStrLn stdout . render . vcat
--  . map (vcat . map content . selection . getElem)) cs

    mapM_ (\x-> do c <- (if x=="-" then getContents else readFile x)
                   ( if isHTML x then
                          hPutStrLn stdout . render . htmlprint .
                          dfilter htmlSelection . getElem . htmlParse x
                     else hPutStrLn stdout . render . vcat . map content .
                          dfilter xmlSelection  . getElem . xmlParse x) c)
          files

getElem (Document _ _ e) = CElem e
isHTML x = ".html" `isSuffixOf` x  ||  ".htm"  `isSuffixOf` x

dfilter f = \x-> f x x
