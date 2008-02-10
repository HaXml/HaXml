module Main where

import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import qualified Text.XML.HaXml.Pretty as PP
import qualified Text.XML.HaXml.Parse as P
import Text.PrettyPrint.HughesPJ
import Text.UTF8

import Debug.Trace
import Test.QuickCheck
import System.IO.Unsafe

main :: IO ()
main = do
  quickCheck prop_reparse

instance Show (Document i) where
  show d = render (PP.document d)

prop_reparse :: Document Posn -> Bool
prop_reparse d =
  str == trace (diff) str'
  where
  str = pp d
  str' = pp d'
  d' = trace (dump) $ P.xmlParse "" $ pp d
  diff =
    let diffstr = zipWith clean str str'
        in if all (==' ') diffstr
              then ""
              else "\n"++(replicate 60 'v')++"\nDIFF:\n"++diffstr++"\n"++(replicate 60 '^')
  clean a b | a == b = ' '
            | otherwise = a
  pp = render . PP.document
  dump = unsafePerformIO $ do { writeFile "generated.xml" (toUTF8 str) ; return "" }
