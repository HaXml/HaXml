module SymTab where

-- A simple symbol table for storing macros whilst parsing.


---- Symbol table stuff ----
type SymTab a = [(String,a)]

emptyST :: SymTab a
emptyST  = []

addST :: String -> a -> SymTab a -> SymTab a
addST n v = ((n,v):)

lookupST :: String -> SymTab a -> Maybe a
lookupST = lookup

