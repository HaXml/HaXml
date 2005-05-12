-- utilities for writing new rules.

module RuleUtils (module Pretty,module RuleUtils, module DataP)where

import Pretty
import DataP (Statement(..),Data(..),Type(..),Name,Var,Class,
		Body(..),Constructor)


-- New Pretty Printers ---------------

texts :: [String] -> [Doc]
texts = map text

block, blockList,parenList,bracketList :: [Doc] -> Doc
block = nest 4 . vcat
blockList = braces . fcat . sepWith semi 
parenList = parens . fcat . sepWith comma
bracketList = brackets . fcat . sepWith comma

-- for bulding m1 >> m2 >> m3, f . g . h, etc
sepWith :: a -> [a] -> [a]
sepWith _ [] = []
sepWith a [x] = [x]
sepWith a (x:xs) = x:a: sepWith a xs

--optional combinator, applys fn if arg is non-[]
opt :: [a] -> ([a] -> Doc) -> Doc
opt [] f = empty
opt a f = f a

--equivalent of `opt' for singleton lists
opt1 :: [a] -> ([a] -> Doc) -> (a -> Doc) -> Doc
opt1 [] _ _ = empty
opt1 [x] _ g = g x
opt1 a f g = f a 

-- new simple docs
commentLine x = text "--" <+> x -- useful for warnings / error messages
commentBlock x = text "{-" <> x <> text "-}"

--- Utility Functions -------------------------------------------------------

-- Instances

-- instance header, handling class constraints etc.
simpleInstance :: Class -> Data -> Doc
simpleInstance s d = hsep [text "instance" 
		, opt constr (\x -> parenList x <+> text "=>")
		, text s
		, opt1 (texts (name d : vars d)) parenSpace id]
   where
   constr = map (\(c,v) -> text c <+> text v) (constraints d) ++
		      map (\x -> text s <+> text x) (vars d)	
   parenSpace = parens . hcat . sepWith space


-- instanceSkeleton handles most instance declarations, where instance
-- functions are not related to one another.  A member function is generated 
-- using a (IFunction,Doc) pair.  The IFunction is applied to each body of the
--  type, creating a block of pattern - matching cases. Default cases can be
-- given using the Doc in the pair.  If a default case is not required, set
-- Doc to 'empty'

type IFunction = Body -> Doc -- instance function

instanceSkeleton :: Class -> [(IFunction,Doc)] -> Data -> Doc
instanceSkeleton s ii  d = (simpleInstance s d <+> text "where") 
				$$ block functions
	where
	functions = concatMap f ii
	f (i,dflt) = map i (body d) ++ [dflt]      

-- little variable name generator, generates (length l) unique names aa - aZ
varNames :: [a] -> [Doc]
varNames l = take (length l) names
   where names = [text [x,y] | x <- ['a' .. 'z'], 
                               y <- ['a' .. 'z'] ++ ['A' .. 'Z']]
-- variant generating aa' - aZ'
varNames' :: [a] -> [Doc]
varNames' = map (<> (char '\'')) . varNames

-- pattern matching a constructor and args
pattern :: Constructor -> [a] -> Doc
pattern c l = parens $ fsep (text c : varNames l)

pattern' :: Constructor -> [a] -> Doc
pattern' c l = parens $ fsep (text c : varNames' l)

-- test that a datatype has at least one record constructor
hasRecord :: Data -> Bool
hasRecord d =   statement d == DataStmt
		&& any (not . null . labels) (body d)

