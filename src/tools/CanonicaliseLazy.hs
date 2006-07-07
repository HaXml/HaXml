module Main where

import System (getArgs)
import IO
import List   (isSuffixOf)

import Text.XML.HaXml.ParseLazy  (xmlParse)
import Text.XML.HaXml.Html.ParseLazy (htmlParse)
import Text.XML.HaXml.Pretty     (document)
import Text.XML.HaXml.Wrappers   (fix2Args)
import Text.PrettyPrint.HughesPJ (render)

-- This is just a trivial application that reads an XML or HTML document
-- from a file (or stdin) and writes it back to another file (or stdout).
-- It demonstrates the behaviour of the parser and pretty-printer,
-- including any shortcomings they may have.

main =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )            >>= \content->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  let parse = if ".html" `isSuffixOf` inf || ".htm" `isSuffixOf` inf
              then htmlParse inf else xmlParse inf
  in
  do ( mapM_ (hPutStrLn o) . lines . render . document . parse) content
     hFlush o

