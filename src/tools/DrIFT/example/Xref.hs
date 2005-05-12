import Binary

data Index a = Empty
             | Branch (Index a) string [a] (Index a)
             {-!derive : Binary!-}

