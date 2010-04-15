------------------------------------------------------------
-- The Xtract tool - an XML-grep.
------------------------------------------------------------ 
module Main where
import System (getArgs, exitWith, ExitCode(..))
import IO
import Char   (toLower)
import List   (isSuffixOf)
import Monad  (when)

import Text.XML.HaXml               (version)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn          (posInNewCxt)
import Text.XML.HaXml.Parse         (xmlParse)
import Text.XML.HaXml.Html.Parse    (htmlParse)
import Text.XML.HaXml.Xtract.Parse  (xtract)
import Text.PrettyPrint.HughesPJ    (Doc,render, vcat, hcat, empty)
import Text.XML.HaXml.Pretty        (content)
import Text.XML.HaXml.Html.Generate (htmlprint)
import Text.XML.HaXml.Escape        (xmlEscapeContent,stdXmlEscaper)
import Text.XML.HaXml.Util          (docContent)

escape :: [Content i] -> [Content i]
escape = xmlEscapeContent stdXmlEscaper

main :: IO ()
main = do
  args <- getArgs
  when ("--version" `elem` args) $ do
      putStrLn $ "part of HaXml-"++version
      exitWith ExitSuccess
  when ("--help" `elem` args) $ do
      putStrLn $ "See http://haskell.org/HaXml"
      exitWith ExitSuccess
  when (length args < 1) $ do
      putStrLn "Usage: Xtract [-n] <pattern> [xmlfile ...]"
      exitWith (ExitFailure 1)
  let (pattern,files,esc) =
          case args of ("-n":pat:files) -> (pat,files, (:[]))
                       (pat:"-n":files) -> (pat,files, (:[]))
                       (pat:files)      -> (pat,files, escape.(:[]))
--      findcontents =
--        if null files then (getContents >>= \x-> return [xmlParse "<stdin>"x])
--        else mapM (\x-> do c <- (if x=="-" then getContents else readFile x)
--                           return ((if isHTML x
--                                    then htmlParse x else xmlParse x) c))
--                  files
--  findcontents >>= \cs->
--  ( hPutStrLn stdout . render . vcat
--  . map (vcat . map content . selection . docContent)) cs
  mapM_ (\x->   do c <- (if x=="-" then getContents else readFile x)
                   ( if isHTML x then
                          hPutStrLn stdout . render . htmlprint
                          . xtract (map toLower) pattern
                          . docContent (posInNewCxt x Nothing) . htmlParse x
                     else hPutStrLn stdout . render . vcat . map (format . esc)
                          . xtract id pattern
                          . docContent (posInNewCxt x Nothing) . xmlParse x) c
                   hFlush stdout)
          files

isHTML :: [Char] -> Bool
isHTML x = ".html" `isSuffixOf` x  ||  ".htm"  `isSuffixOf` x

format :: [Content i] -> Doc
format [] = empty
format cs@(CString _ _ _:_) = hcat . map content $ cs
format cs@(CRef _ _:_)      = hcat . map content $ cs
format cs                   = vcat . map content $ cs
