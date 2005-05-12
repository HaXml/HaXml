module Example where

import Binary

data MyType a = ConsA Int (U (Int,[a]))
              | ConsB String --a
              | Red
              | Blue Int String {-(MyType a)-} [Int]
              {-!derive : Binary !-}


