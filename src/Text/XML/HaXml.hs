module Text.Xml.HaXml
  ( module Text.Xml.HaXml.Types
  , module Text.Xml.HaXml.Combinators
  , module Text.Xml.HaXml.Parse
  , module Text.Xml.HaXml.Pretty
  , module Text.Xml.HaXml.Html.Generate
  , module Text.Xml.HaXml.Html.Parse
  , module Text.Xml.HaXml.Html.Pretty
  , module Text.Xml.HaXml.Validate
  , processXmlWith
  , fix2Args
  ) where

-- This is just a convenient way of bunching the XML combinators
-- together with some other things you are likely to want at the
-- same time.

import Text.Xml.HaXml.Types
import Text.Xml.HaXml.Combinators
import Text.Xml.HaXml.Html.Generate
import Text.Xml.HaXml.Validate


-- imports required for processXmlWith and fix2Args
import System
import IO
import List (isSuffixOf)

import Text.Xml.HaXml.Parse       (xmlParse)
import Text.Xml.HaXml.Pretty      (element)
import Text.Xml.HaXml.Html.Parse  (htmlParse)
import Text.Xml.HaXml.Html.Pretty ()
import Text.PrettyPrint.HughesPJ  (render)


-- | The wrapper @processXmlWith@ returns an IO () computation
--   that collects the filenames (or stdin\/stdout) to use when
--   reading\/writing XML documents.  Its CFilter argument
--   is applied to transform the XML document from the input and
--   write it to the output.

processXmlWith :: CFilter -> IO ()
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


-- | This useful auxiliary checks the commandline arguments for two
--   filenames, the input and output file respectively.  If either
--   is missing, or is '-', then stdin and\/or stdout is used in its place.
fix2Args :: IO (String,String)
fix2Args = do
  args <- getArgs
  case length args of
    0 -> return ("-",     "-")
    1 -> return (args!!0, "-")
    2 -> return (args!!0, args!!1)
    _ -> do prog <- getProgName
            putStrLn ("Usage: "++prog++" [xmlfile] [outfile]")
            exitFailure

