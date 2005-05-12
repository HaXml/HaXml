module Main where

-- This program is provided to convert an XML file containing a DTD
-- into a Haskell module containing data/newtype definitions which
-- mirror the DTD.  Once you have used this program to generate your type
-- definitions, you should import Xml2Haskell wherever you intend
-- to read and write XML files with your Haskell programs.

import System
import IO
import List (nub,takeWhile,dropWhile)

import XmlTypes
import XmlLib    (fix2Args)
import XmlParse  (dtdParse)
import DtdToTypeDefPP
import Pretty (render,vcat)
#if defined(__HBC__)
import IOMisc (hPutStrLn)
#endif

main =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )           >>= \content->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  let (DTD name _ markup) = (getDtd . dtdParse inf) content
      decls = (nub . dtd2typedef) markup
      realname = if null name then mangle (trim inf) else mangle name
  in
  do hPutStrLn o ("module DTD_"++realname++" where\n\nimport Xml2Haskell")
     hPutStrLn o "\n\n{-Type decls-}\n"
     (hPutStrLn o . render . vcat . map ppTypeDef) decls
     hPutStrLn o "\n\n{-Instance decls-}\n"
     (hPutStrLn o . render . vcat . map mkInstance) decls
     hPutStrLn o "\n\n{-Done-}"


getDtd (Just dtd) = dtd
getDtd (Nothing)  = error "No DTD in this document"

trim name | '/' `elem` name  = (trim . tail . dropWhile (/='/')) name
          | '.' `elem` name  = takeWhile (/='.') name
          | otherwise        = name

--render = foldr (.) id . map showsTypeDef . nub
