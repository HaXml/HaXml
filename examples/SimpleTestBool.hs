module Main where

import List (isPrefixOf)
import Text.Xml.HaXml.Haskell2Xml
import Text.Xml.HaXml.Types
import Text.PrettyPrint.HughesPJ (render)
import Text.Xml.HaXml.Pretty     (document)

-- Test stuff
--value1 :: ([(Bool,Int)],(String,Maybe Char))
value1 = True

--main = do (putStrLn . render . document . toXml) value2

main = writeXml "/dev/tty" value1
        
