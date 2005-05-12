module XmlCombinators
  ( CFilter
  , find, iffind, ifTxt, cat
  , (|>|), with, without, o, union
  , (/>), (</), deep, deepest, multi, when, guards
  , chip, foldXml, et
  , keep, none, children
  , elm, txt, tag, attr, attrval, tagWith
  , mkElem, mkElemAttr, literal, cdata, replaceTag, replaceAttrs
  , (?>), ThenElse(..)
  , LabelFilter
  , oo, x, numbered, interspersed, tagged, attributed, textlabelled
  , position, combine
  ) where

-- These XML transformation combinators are described in our paper in
-- the ICFP'99 proceedings.

import XmlTypes
import Maybe (fromMaybe)

infixl 6 `with`, `without`
infixr 5 `o`, `oo`, `union`, `andThen`		-- , `orelse`
infixl 5 />, </, |>|
infixr 4 `when`, `guards`
infixr 3 ?>, :>

--------------------------------------------
-- This module defines some filters, filter combinators, etc,
-- for processing XML documents.
--------------------------------------------

type CFilter    = Content -> [Content]

lift :: (a->b->d) -> (c->a) -> (c->b) -> c -> d
lift f g h = \x-> f (g x) (h x)

-- When an attribute field is mandatory, find its value and continue.
find :: String -> (String->CFilter) -> CFilter
find key cont c@(CElem (Elem _ as _)) = cont (value (lookfor key as)) c
  where lookfor x = fromMaybe (error ("missing attribute: "++show x)) . lookup x
        value (AttValue [Left x]) = x
-- 'lookfor' has the more general type :: (Eq a,Show a) => a -> [(a,b)] -> b

-- When an attribute field may not be present, find its value and continue.
iffind :: String -> (String->CFilter) -> CFilter -> CFilter
iffind key yes no c@(CElem (Elem _ as _)) =
  case (lookup key as) of
    Nothing  -> no c
    (Just (AttValue [Left s])) -> yes s c
iffind key yes no other = no other

-- When you want to process the txt content.
ifTxt :: (String->CFilter) -> CFilter -> CFilter
ifTxt yes no c@(CString _ s) = yes s c
ifTxt yes no c               = no c



-- C-like CONDITIONALS, (lifted to filter level).

data ThenElse a = a :> a

(?>) :: (a->[b]) -> ThenElse (a->[b]) -> (a->[b])
p ?> (f :> g) = \c-> if (not.null.p) c then f c else g c



-- FILTER COMBINATORS

-- sequential composition
o :: CFilter -> CFilter -> CFilter
f `o` g = concatMap f . g

-- binary parallel composition (each filter uses copy of state).
union :: (a->[b]) -> (a->[b]) -> (a->[b])
union = lift (++)		-- in Haskell 98:   union = lift List.union

-- stick lots of filters together.  (A list version of union.)
-- specification: cat fs = \e-> concat [ f e | f <- fs ]
-- more efficient implementation below:
cat :: [a->[b]] -> (a->[b])
cat [] = const []
cat fs = foldr1 (lift (++)) fs
         where lift f g h x = f (g x) (h x)

-- A special form of filter composition where the second filter
-- works over the same data as the first, but also uses the
-- first's result.
andThen :: (a->c) -> (c->a->b) -> (a->b)
andThen f g = \x-> g (f x) x			-- lift g f id

-- Process children using specified filters.
childrenBy :: CFilter -> CFilter
childrenBy f = f `o` children

-- directional choice: give g-productions only if no f-productions
(|>|) :: (a->[b]) -> (a->[b]) -> (a->[b])
f |>| g = \x-> let fx = f x in if null fx then g x else fx
--      f |>| g  =  f ?> f :> g

-- pruning: keep only those f-productions which have at least one g-production
f `with` g = filter (not.null.g) . f

-- pruning: keep only those f-productions which have no g-productions
f `without` g = filter (null.g) . f

-- pronounced "slash", meaning g inside f
(/>) :: CFilter -> CFilter -> CFilter
f /> g = g `o` children `o` f

-- pronounced "outside" meaning f containing g
(</) :: CFilter -> CFilter -> CFilter
f </ g = f `with` (g `o` children)

-- recursive search
deep :: CFilter -> CFilter
deep f     = f |>| (deep f `o` children)
deepest f  = (deepest f `o` children) |>| f
multi f    = f `union` (multi f `o` children)

-- interior editing
when :: CFilter -> CFilter -> CFilter
f `when` g       = g ?> f :> keep
g `guards` f     = g ?> f :> none	-- = f `o` (keep `with` g)

-- process CHildren In Place
chip :: CFilter -> CFilter
chip f (CElem (Elem n as cs)) = [ CElem (Elem n as (concatMap f cs)) ]
chip f c = [c]

-- recursive application of filters: a fold-like operator
foldXml :: CFilter -> CFilter
foldXml f = f `o` chip (foldXml f)

-- join an element-matching filter with a text-only filter
et :: (String->CFilter) -> CFilter -> CFilter
et f g = (f `oo` tagged elm)
            |>|
         (g `o` txt)



-- CONTENT FILTERS

-- Some basic filters for content.  (zero and identity in the algebra.)
keep, none :: a->[a]	-- generalisation of CFilter
keep = \x->[x]
none = \x->[]

-- Throw away current node, keep just the (unprocessed) children.
children :: CFilter
children (CElem (Elem _ _ cs)) = cs
children _ = []

-- Other basic content filters.  (These five are predicates.)
elm, txt   :: CFilter
tag        :: String -> CFilter
attr       :: Name -> CFilter
attrval    :: Attribute -> CFilter
tagWith    :: (String->Bool) -> CFilter


elm x@(CElem _)   = [x]
elm _             = []

txt x@(CString _ _) = [x]
txt x@(CRef _)      = [x]
txt _               = []

tag t x@(CElem (Elem n _ _)) | t==n  = [x]
tag t _  = []

tagWith p x@(CElem (Elem n _ _)) | p n  = [x]
tagWith p _  = []

attr n x@(CElem (Elem _ as _)) | n `elem` (map fst as)  = [x]
attr n _  = []

attrval av x@(CElem (Elem _ as _)) | av `elem` as  = [x]
attrval av _  = []


-- CONSTRUCTIVE CONTENT FILTERS

mkElem :: String -> [CFilter] -> CFilter
mkElem h cfs = \t-> [ CElem (Elem h [] (cat cfs t)) ]

mkElemAttr :: String -> [(String,CFilter)] -> [CFilter] -> CFilter
mkElemAttr h as cfs = \t-> [ CElem (Elem h (map (attr t) as) (cat cfs t)) ]
  where attr t (n,vf) =
            let v = concat [ s | (CString _ s) <- (deep txt `o` vf) t ]
            in  (n, AttValue [Left v])

literal :: String -> CFilter
literal s = const [CString False s]

cdata :: String -> CFilter
cdata s = const [CString True s]

replaceTag :: String -> CFilter
replaceTag n (CElem (Elem _ _ cs)) = [CElem (Elem n [] cs)]
replaceTag n _ = []

replaceAttrs :: [(String,String)] -> CFilter
replaceAttrs as (CElem (Elem n _ cs)) = [CElem (Elem n as' cs)]
    where as' = map (\(n,v)-> (n, AttValue [Left v])) as
replaceAttrs as _ = []



-- LABELLING  *** Visible. ***

type LabelFilter a = Content -> [(a,Content)]

-- Compose a label-processing filter and a label-generating filter.
oo :: (a->CFilter) -> LabelFilter a -> CFilter
f `oo` g = concatMap (uncurry f) . g

-- Some basic label-generating filters.
numbered :: CFilter -> LabelFilter String
numbered f = zip (map show [(1::Int)..]) . f

interspersed :: String -> CFilter -> String -> LabelFilter String
interspersed a f b =
  (\xs-> zip (replicate (len xs) a ++ [b]) xs) . f
  where
  len [] = 0
  len xs = length xs - 1

tagged :: CFilter -> LabelFilter String
tagged f = concatMap getName . f
  where getName c@(CElem (Elem n _ _)) = [(n,c)]
        getName c                      = [("",c)]

attributed :: String -> CFilter -> LabelFilter String
attributed key f = concatMap getName . f
  where getName c@(CElem (Elem _ as _)) =
            case (lookup key as) of
              Nothing  -> [("",c)]
              (Just (AttValue [Left s])) -> [(s,c)]
        getName c = [("",c)]

textlabelled :: CFilter -> LabelFilter (Maybe String)
textlabelled f = concatMap getText . f
  where getText c@(CString _ s) = [(Just s,c)]
        getText c = [(Nothing,c)]


-- Combine labels:
x :: (CFilter->LabelFilter a) -> (CFilter->LabelFilter b) ->
       (CFilter->LabelFilter (a,b))
f `x` g = \cf c-> let gs = map fst (g cf c)
                      fs = map fst (f cf c)
                  in zip (zip fs gs) (cf c)


-- MISC

position :: Int -> CFilter -> CFilter
position n f = (\cs-> [cs!!n]) . f

combine :: (Read a,Show a) => ([a]->a) -> LabelFilter String -> CFilter
combine f lf = \c-> [ CString False (show (f [ read l | (l,_) <- lf c ])) ]



{- OLD STUFF - OBSOLETE
-- Keep an element by its numbered position (starting at 1).
position :: Int -> [Content] -> [Content]
position n | n>0  = (:[]) . (!!(n-1))
           | otherwise = const []

-- Chop and remove the root portions of trees to depth n.
layer :: Int -> [Content] -> [Content]
layer n = apply n (concatMap lay)
  where lay (CElem (Elem _ _ cs)) = cs
        lay _ = []
        apply 0 f xs = xs
        apply n f xs = apply (n-1) f (f xs)

combine :: (Read a, Show a) => ([a]->a) -> [Content] -> [Content]
combine f = \cs-> [ CString False (show (f [ read s | CString _ s <- cs ])) ]
-}
