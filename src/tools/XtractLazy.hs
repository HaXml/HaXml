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
import Text.XML.HaXml.Posn          (posInNewCxt,Posn)
import Text.XML.HaXml.ParseLazy     (xmlParse)
import Text.XML.HaXml.Html.ParseLazy(htmlParse)
import Text.XML.HaXml.Xtract.Parse  (xtract)
import Text.PrettyPrint.HughesPJ    (Doc,render, vcat, hcat, empty)
import Text.XML.HaXml.Pretty        (content)
import Text.XML.HaXml.Html.Generate (htmlprint)
import Text.XML.HaXml.Escape        (xmlEscapeContent,stdXmlEscaper)

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
--  . map (vcat . map content . selection . getElem)) cs
  mapM_ (\x->   do c <- (if x=="-" then getContents else readFile x)
                   ( if isHTML x then
                          hPutStrLn stdout . render . htmlprint
                          . xtract (map toLower) pattern
                          . getElem x . htmlParse x
                     else hPutStrLn stdout . render . vcat . map (format . esc)
                          . xtract id pattern
                          . getElem x . xmlParse x) c
                   hFlush stdout)
          files

getElem :: String -> Document Posn -> Content Posn
getElem x (Document _ _ e _) = CElem e (posInNewCxt x Nothing)

isHTML :: [Char] -> Bool
isHTML x = ".html" `isSuffixOf` x  ||  ".htm"  `isSuffixOf` x

format :: [Content i] -> Doc
format [] = empty
format cs@(CString _ _ _:_) = hcat . map content $ cs
format cs@(CRef _ _:_)      = hcat . map content $ cs
format cs                   = vcat . map content $ cs
