module Text.Xml.HaXml.Xtract.Parse (parseXtract) where

-- A parser for the Xtract command-language.  The input has already been
-- tokenised by the lexer lexXtract.  The parser uses the extended
-- Hutton/Meijer parser combinators.
-- Because the original Xtract grammar was left-recursive, we have
-- transformed it into a non-left-recursive form.

#ifdef __NHC__
import NonStdTrace (trace)
#endif

import Text.ParserCombinators.HuttonMeijerWallace hiding (bracket,elserror)
import Text.Xml.HaXml.Xtract.Lex
import Text.Xml.HaXml.Xtract.Combinators
import Text.Xml.HaXml.Combinators
import List(isPrefixOf)

parseXtract :: String -> DFilter
parseXtract = sanitycheck . papply xql () . lexXtract

sanitycheck :: (Show p,Show t) => [(a,s,[(p,t)])] -> a
sanitycheck [] = error "***Error at char pos 0 in expression: no parse"
sanitycheck ((x,_,[]):_) = x
sanitycheck ((x,_,s@((n,_):_)):xs) =
  error ("***Error at "++show n++" in search expression: \""++remainder++"\"")
  where remainder = concatMap (show.snd) s

xql = aquery (global keep)


---- Auxiliary Parsing Functions ----

string :: Parser s Token String
string = P (\st inp -> case inp of {
                        ((p,TokString n):ts) -> [(n,st,ts)];
                        ts -> [] } )
number :: Parser s Token Integer
number = P (\st inp -> case inp of {
                        ((p,TokNum n):ts) -> [(n,st,ts)];
                        ts -> [] } )
symbol :: String -> Parser s Token ()
symbol s = P (\st inp -> case inp of {
                          ((p, Symbol n):ts) -> if n==s then [((),st,ts)] else [];
                          ts -> [] } )
quote = symbol "'" +++ symbol "\""

pam fs x = [ f x | f <- fs ]


{--- original Xtract grammar ----
      query     = string			tagname
                | string *			tagname prefix
                | * string			tagname suffix
                | *				any element
                | -				chardata
                | ( query )
                | query / query			parent/child relationship
                | query // query		deep inside
                | query + query			union of queries
                | query [predicate]
                | query [positions]

      predicate = quattr			has attribute
                | quattr op ' string '		attribute has value
                | quattr op " string "		attribute has value
                | quattr op  quattr		attribute value comparison (lexical)
                | quattr nop integer  		attribute has value (numerical)
                | quattr nop quattr		attribute value comparison (numerical)
                | ( predicate )			bracketting
                | predicate & predicate		logical and
                | predicate | predicate		logical or
                | ~ predicate			logical not

      attribute = @ string			has attribute
                | query / @ string		child has attribute
                | -				has textual content
                | query / -			child has textual content

      quattr    = query
                | attribute

      op        =  =				equal to
                |  !=				not equal to
                |  <				less than
                |  <=				less than or equal to
                |  >				greater than
                |  >=				greater than or equal to

      nop       =  .=.				equal to
                |  .!=.				not equal to
                |  .<.				less than
                |  .<=.				less than or equal to
                |  .>.				greater than
                |  .>=.				greater than or equal to

      positions = position {, positions}	multiple positions
                | position - position		ranges

      position  = integer			numbering is from 0 upwards
                | $				last


---- transformed grammar (removing left recursion)
      aquery = ./ tquery	-- current context
             | tquery		-- also current context
             | / tquery		-- root context
             | // tquery	-- deep context from root

      tquery = ( tquery ) xquery
             | tag xquery
             | -		-- fixes original grammar ("-/*" is incorrect)
      
      tag    = string *
             | string
             | * string
             | *
      
      xquery = / tquery
             | // tquery
             | / @ string	-- new: print attribute value
             | + tquery
             | [ tpredicate ] xquery
             | [ positions ] xquery
             | lambda

      tpredicate = vpredicate upredicate
      upredicate = & tpredicate
                 | | tpredicate
                 | lambda
      vpredicate = ( tpredicate )
                 | ~ tpredicate
                 | tattribute

      tattribute = aquery uattribute
                 | @ string vattribute
      uattribute = / @ string vattribute
                 | vattribute
      vattribute = op wattribute
                 | op ' string '
                 | nop wattribute
                 | nop integer
                 | lambda
      wattribute = @ string
                 | aquery / @ string
                 | aquery

      positions  = simplepos commapos
      simplepos  = integer range
                 | $
      range      = - integer
                 | - $
                 | lambda
      commapos   = , simplepos commapos
                 | lambda

      op         =  =
                 |  !=
                 |  <
                 |  <=
                 |  >
                 |  >=

      nop        =  .=.
                 |  .!=.
                 |  .<.
                 |  .<=.
                 |  .>.
                 |  .>=.
-}

bracket :: Parser s Token a -> Parser s Token a
bracket p =
  do symbol "("
     x <- p
     symbol ")"
     return x


---- Xtract parsers ----

-- aquery takes a localisation filter, but if the query string starts
-- from the root, we ignore the local context
aquery :: DFilter -> Parser s Token DFilter
aquery localise =
  ( do symbol "//"
       tquery [oglobo deep] ) +++
  ( do symbol "/"
       tquery [oglobo id] ) +++
  ( do symbol "./"
       tquery [(localise //>>)] ) +++
  ( do tquery [(localise //>>)] )

tquery :: [DFilter->DFilter] -> Parser s Token DFilter
tquery [] = tquery [id]
tquery (qf:cxt) =
  ( do q <- bracket (tquery (qf:qf:cxt))
       xquery cxt q ) +++
  ( do q <- xtag
       xquery cxt (qf q) ) +++
  ( do symbol "-"
       return (qf (local txt)) )

xtag :: Parser s Token DFilter
xtag =
  ( do s <- string
       symbol "*"
       return (local (tagWith (s `isPrefixOf`))) ) +++
  ( do s <- string
       return (local (tag s)) ) +++
  ( do symbol "*"
       s <- string
       return (local (tagWith (((reverse s) `isPrefixOf`) . reverse))) ) +++
  ( do symbol "*"
       return (local elm) )


xquery :: [DFilter->DFilter] -> DFilter -> Parser s Token DFilter
xquery cxt q1 =
  ( do symbol "/"
       ((do symbol "@"
            attr <- string
            return (oiffindo attr (\s->local (literal s)) ononeo `ooo` q1))
        +++
        tquery ((q1 //>>):cxt)) ) +++
  ( do symbol "//"
       tquery ((\q2-> (oloco deep) q2 `ooo` local children `ooo` q1):cxt) ) +++
  ( do symbol "+"
       q2 <- tquery cxt
       return (ocato [q1,q2]) ) +++
  ( do symbol "["
       is <- iindex	-- now extended to multiple indexes
       symbol "]"
       xquery cxt (\xml-> concat . pam is . q1 xml) ) +++
  ( do symbol "["
       p <- tpredicate
       symbol "]"
       xquery cxt (q1 `owitho` p) ) +++
  ( do return q1 )

tpredicate :: Parser s Token DFilter
tpredicate =
  do p <- vpredicate
     f <- upredicate
     return (f p)

upredicate :: Parser s Token (DFilter->DFilter)
upredicate =
  ( do symbol "&"
       p2 <- tpredicate
       return (`ooo` p2) ) +++
  ( do symbol "|"
       p2 <- tpredicate
       return (||>|| p2) ) +++
  ( do return id )

vpredicate :: Parser s Token DFilter
vpredicate =
  ( do bracket tpredicate ) +++
  ( do symbol "~"
       p <- tpredicate
       return (local keep `owithouto` p) ) +++
  ( do tattribute )

tattribute :: Parser s Token DFilter
tattribute =
  ( do q <- aquery (local keep)
       uattribute q ) +++
  ( do symbol "@"
       s <- string
       vattribute (local keep, local (attr s), oiffindo s) )

uattribute :: DFilter -> Parser s Token DFilter
uattribute q =
  ( do symbol "/"
       symbol "@"
       s <- string
       vattribute (q, local (attr s), oiffindo s) ) +++
  ( do vattribute (q, local keep,     oifTxto) )

vattribute :: (DFilter, DFilter, (String->DFilter)->DFilter->DFilter)
              -> Parser s Token DFilter
vattribute (q,a,iffn) =
  ( do cmp <- op
       quote
       s2 <- string
       quote
       return ((iffn (\s1->if cmp s1 s2 then okeepo else ononeo) ononeo)
               `ooo` q) ) +++
  ( do cmp <- op
       (q2,iffn2) <- wattribute
       return ((iffn (\s1-> iffn2 (\s2-> if cmp s1 s2 then okeepo else ononeo)
                                  ononeo)
                     ononeo) `ooo` q)
              ) +++
  ( do cmp <- nop
       n <- number
       return ((iffn (\s->if cmp (read s) n then okeepo else ononeo) ononeo)
               `ooo` q) ) +++
  ( do cmp <- nop
       (q2,iffn2) <- wattribute
       return ((iffn (\s1-> iffn2 (\s2-> if cmp (read s1) (read s2) then okeepo
                                                                    else ononeo)
                                  ononeo)
                     ononeo) `ooo` q) ) +++
  ( do return ((a `ooo` q)))

wattribute :: Parser s Token (DFilter, (String->DFilter)->DFilter->DFilter)
wattribute =
  ( do symbol "@"
       s <- string
       return (okeepo, oiffindo s) ) +++
  ( do q <- aquery okeepo
       symbol "/"
       symbol "@"
       s <- string
       return (q, oiffindo s) ) +++
  ( do q <- aquery okeepo
       return (q, oifTxto) )


iindex :: Parser s Token [[a]->[a]]
iindex =
  do i <- simpleindex
     is <- idxcomma
     return (i:is)

simpleindex :: Parser s Token ([a]->[a])
simpleindex =
  ( do n <- number
       r <- rrange n
       return r ) +++
  ( do symbol "$"
       return (keep . last) )

rrange, numberdollar :: Integer -> Parser s Token ([a]->[a])
rrange n1 =
  ( do symbol "-"
       numberdollar n1 ) +++
  ( do return (keep.(!!(fromInteger n1))) )

numberdollar n1 =
  ( do n2 <- number
       return (take (fromInteger (1+n2-n1)) . drop (fromInteger n1)) ) +++
  ( do symbol "$"
       return (drop (fromInteger n1)) )

idxcomma :: Parser s Token [[a]->[a]]
idxcomma =
  ( do symbol ","
       r <- simpleindex
       rs <- idxcomma
       return (r:rs) ) +++
  ( do return [] )


op :: Parser s Token (String->String->Bool)
op =
  ( do symbol "="
       return (==) ) +++
  ( do symbol "!="
       return (/=) ) +++
  ( do symbol "<"
       return (<) ) +++
  ( do symbol "<="
       return (<=) ) +++
  ( do symbol ">"
       return (>) ) +++
  ( do symbol ">="
       return (>=) )

nop :: Parser s Token (Integer->Integer->Bool)
nop =
  ( do symbol ".=."
       return (==) ) +++
  ( do symbol ".!=."
       return (/=) ) +++
  ( do symbol ".<."
       return (<) ) +++
  ( do symbol ".<=."
       return (<=) ) +++
  ( do symbol ".>."
       return (>) ) +++
  ( do symbol ".>=."
       return (>=) )

