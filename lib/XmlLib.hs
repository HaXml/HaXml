module XmlLib
  ( module XmlTypes
  , module XmlCombinators
  , module XmlHtmlGen
  , processXMLwith
  , processXmlWith
  , fix2Args
  ) where

-- This is just a convenient way of bunching the XML combinators
-- together with some other things you are likely to want at the
-- same time.

import XmlTypes
import XmlCombinators
import XmlHtmlGen


-- imports required for processXmlWith and fix2Args
import System
import IO
#if defined(__HBC__)
import IOMisc (hPutStrLn)
#endif

import XmlParse     (xmlParse)
import XmlHtmlParse (htmlParse)
import XmlPP        (element)
import Pretty       (render)
#if !defined(__HASKELL98__)
import IsSuffixOf
import ExitFailure
#else
import List (isSuffixOf)
#endif


processXMLwith = processXmlWith		-- synonym for backward compat.


-- This wrapper, processXmlWith, provides an IO () computation
-- that collects the filenames (or stdin/stdout) to use when
-- reading/writing XML documents.  In between, its CFilter argument
-- is applied to transform the XML document.

processXmlWith f =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )            >>= \input->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  let (parse,header) = if ".html" `isSuffixOf` inf || ".htm" `isSuffixOf` inf
                       then (htmlParse inf, return ())
                       else (xmlParse inf,  hPutStrLn o "<?xml version='1.0'?>")
  in
  header >>
  ( hPutStrLn o . render . ppContent . f . getContent . parse) input

getContent (Document _ _ e) = CElem e

ppContent [CElem e] = element e
ppContent []  = error "produced no output"
ppContent _   = error "produced more than one output"


fix2Args = do
  args <- getArgs
  case length args of
    0 -> return ("-",     "-")
    1 -> return (args!!0, "-")
    2 -> return (args!!0, args!!1)
    _ -> do prog <- getProgName
            putStrLn ("Usage: "++prog++" [xmlfile] [outfile]")
            exitFailure

