------------------------------------------------------------
-- The Xtract tool - an XML-grep.
------------------------------------------------------------ 
module Main where
import System (getArgs, exitWith, ExitCode(..))
import IO
import Char         (toLower)
import List         (isSuffixOf)

import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse         (xmlParse)
import Text.XML.HaXml.Html.Parse    (htmlParse)
import Text.XML.HaXml.Xtract.Parse  (xtract)
import Text.PrettyPrint.HughesPJ    (render, vcat, hcat, empty)
import Text.XML.HaXml.Pretty        (content)
import Text.XML.HaXml.Html.Generate (htmlprint)


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
    in
--  findcontents >>= \cs->
--  ( hPutStrLn stdout . render . vcat
--  . map (vcat . map content . selection . getElem)) cs
    mapM_ (\x-> do c <- (if x=="-" then getContents else readFile x)
                   ( if isHTML x then
                          hPutStrLn stdout . render . htmlprint .
                          xtract (map toLower pattern) . getElem . htmlParse x
                     else hPutStrLn stdout . render . format .
                          xtract pattern . getElem . xmlParse x) c)
          files

getElem (Document _ _ e _) = CElem e
isHTML x = ".html" `isSuffixOf` x  ||  ".htm"  `isSuffixOf` x

format [] = empty
format cs@(CString _ _:_) = hcat . map content $ cs
format cs@(CRef _:_)      = hcat . map content $ cs
format cs                 = vcat . map content $ cs
