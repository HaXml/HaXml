module Main where

import Haskell2Xml
import XmlTypes
import List (isPrefixOf)
import Pretty (render)
import XmlPP (document)

-- Test stuff
--value1 :: ([(Bool,Int)],(String,Maybe Char))
value1 = True

--main = do (putStrLn . render . document . toXml) value2

main = writeXml "/dev/tty" value1
        
