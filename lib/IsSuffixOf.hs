module IsSuffixOf where

#if defined(__HASKELL98__)
import List (isPrefixOf)
#else
import IsPrefixOf
#endif

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf as bs = (reverse as) `isPrefixOf` (reverse bs)

