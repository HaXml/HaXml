module IsPrefixOf where

-- Only needed by pre-Haskell'98 compilers.

isPrefixOf               :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _           =  True
isPrefixOf (x:xs) (y:ys)  =  x == y && isPrefixOf xs ys

