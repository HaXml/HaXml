import Binary

data BTree k d = BTree Int [(k,[d])] [BTree k d]
           {-! derive : Binary !-}
