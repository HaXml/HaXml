------------------------------------------------------------
-- The Xtract tool - an XML-grep.
------------------------------------------------------------ 
module Main where
import System.Environment (getArgs)
import System.Exit        (exitWith, ExitCode(..))
import System.IO
import Data.Char          (toLower)
import Data.List          (isSuffixOf)

import Text.CSV.Lazy.String

import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn          (posInNewCxt,Posn)
import Text.XML.HaXml.ParseLazy     (xmlParse)
import Text.XML.HaXml.Xtract.Parse  (xtract)
import Text.XML.HaXml.Pretty        (content)
import Text.XML.HaXml.Escape        (xmlUnEscapeContent,stdXmlEscaper)
import Text.XML.HaXml.Verbatim      (verbatim)

main :: IO ()
main =
  getArgs >>= \args->
  when ("--version" `elem` args) $ do
      putStrLn $ "part of HaXml-"++version
      exitWith ExitSuccess
  when ("--help" `elem` args) $ do
      putStrLn $ "See http://haskell.org/HaXml"
      exitWith ExitSuccess

  if length args < 4 then do
    putStrLn "Usage: flattenXMLtoCSV file.xml file.csv tag innertag [innertag ...]  "
    exitWith (ExitFailure 1)
  else do
    let (ifile:ofile:tag:patterns) = args
    -- 'patterns' is the header row of the csv file, consisting of tag names
    -- to be found nested inside the specified outer 'tag'.  Each outer 'tag'
    -- found therefore forms a row of the csv output.
    writeFile (csv ofile) . ppCSVTable
                          . ((map (mkCSVField 0 0) patterns):)
                          . map (\o-> map (\i-> mkCSVField 0 0
                                                . verbatim
                                                . unescape
                                                . xtract id ("//"++i++"/-")
                                                $ o)
                                          patterns)
                          . xtract id ("//"++tag)
                          . getElem ifile . xmlParse ifile =<< readFile ifile

getElem :: String -> Document Posn -> Content Posn
getElem x (Document _ _ e _) = CElem e (posInNewCxt x Nothing)

csv :: FilePath -> FilePath
csv fp | ".xml" `isSuffixOf` fp = (++".csv") . reverse . drop 4 . reverse $ fp
       | ".csv" `isSuffixOf` fp = fp
       | otherwise              = fp++".csv"

unescape :: [Content i] -> [Content i]
unescape = xmlUnEscapeContent stdXmlEscaper

{-
checkOne :: [Content i] -> String
checkOne []              = ""
checkOne [CString _ x _] = x
checkOne _               = verbatim
-}

