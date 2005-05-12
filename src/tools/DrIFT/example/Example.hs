{- example script for derive -}

module Example where
import Foo

{-! global : has,is !-} -- global to this module
{-!for Data derive : update,Show,Read!-} -- stand alone comand syntax 
{-!for Foo derive : test, Show,Read !-} -- apply rules to imported type
{-!for Maybe derive : test !-} -- apply rules to prelude type

data Data = D {	name :: Name,		
			constraints :: [(Class,Var)], 
			vars :: [Var],	
			body :: [(Constructor,[(Name,Type)])],
			derive :: [Class],
			statement :: Statement}
	   | FnType {	name :: Name,
			constraints :: [(Class,Var)],
			fntype :: Type}
	   | Fn	{	name :: Name,
			vars :: [Var]}	
	   | Directive
		 {-!derive : test!-} -- abbreviated syntax
{-!for Statement derive : Eq,Ord,Enum,Show,Read,Bounded !-}
{-!for Type derive : Eq,Ord,Enum,Bounded,Read !-}
data Statement = DataStmt | NewTypeStmt
type Name = String
type Var = String
type Class = String
type Constructor = String

data Type	= Arrow Type Type -- fn
		| Apply Type Type -- application
		| Var String	  -- variable
		| Con String      -- constructor / type e.g. Int, Char
		| Tuple [Type]	  -- tuple
		| List Type	  -- list
			 deriving Show

data (Eq a) => G a b = F (a->b) b | H a a {-!derive: test !-}
newtype Q = Q Int {-!derive:test!-}

