module Main where

import System (getArgs)
import IO

import Text.Xml.HaXml.Lex     (xmlLex)
import Text.Xml.HaXml         (fix2Args)

-- Debug the HaXml library by showing what the lexer generates.
main =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )            >>= \content->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  mapM_ ( hPutStrLn o . show ) (xmlLex inf content)

