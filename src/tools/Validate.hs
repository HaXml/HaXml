module Main where

import System (getArgs)
import IO

import XmlTypes     (Document(..),Content(..))
import XmlParse     (xmlParse,dtdParse)
import XmlLib       (fix2Args)
import XmlValidate  (validate)
import List         (isSuffixOf)
import Maybe        (fromJust)

-- This is a fairly trivial application that reads a DTD from a file,
-- an XML document from another file (or stdin), and writes any validation
-- errors to stdout.

main = do
  (dtdf,xmlf) <- fix2Args
  dtdtext     <- ( if dtdf=="-" then error "Usage: validate dtdfile [xmlfile]"
                   else readFile dtdf )
  content     <- ( if xmlf=="-" then getContents else readFile xmlf )
  let dtd  = dtdParse dtdf dtdtext
      Document _ _ xml  = xmlParse xmlf content
      errs = validate (fromJust dtd) (CElem xml)
  mapM_ putStrLn errs

