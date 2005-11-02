module Text.ParserCombinators.Poly
  ( -- * A Parser datatype parameterised on arbitrary token type and state type
    Parser(P)	-- datatype, instance of: Functor, Monad
  , runParser	-- :: Parser s t a -> s -> [t] -> (Either String a, s, [t])
    -- * Combinators
    -- ** primitives
  , next	-- :: Parser s t t
  , satisfy	-- :: (t->Bool) -> Parser s t t
  , apply	-- :: Parser t (a->b) -> Parser s t a -> Parser s t b
  , discard	-- :: Parser s t a -> Parser s t b -> Parser s t a
    -- ** error-handling
  , adjustErr	-- :: Parser s t a -> (String->String) -> Parser s t a
    -- ** choices
  , onFail	-- :: Parser s t a -> Parser s t a -> Parser s t a
  , oneOf	-- :: [Parser s t a] -> Parser s t a
    -- ** sequences
  , many	-- :: Parser s t a -> Parser s t [a]
  , many1	-- :: Parser s t a -> Parser s t [a]
  , sepBy	-- :: Parser s t a -> Parser s t sep -> Parser s t [a]
  , sepBy1	-- :: Parser s t a -> Parser s t sep -> Parser s t [a]
  , bracketSep	-- :: Parser s t bra -> Parser s t sep -> Parser s t ket
                --    -> Parser s t a -> Parser s t [a]
  , bracket	-- :: Parser s t bra -> Parser s t ket -> Parser s t a
                --    -> Parser s t a
    -- ** state-handling
  , stUpdate	-- :: (s->s) -> Parser s t ()
  , stQuery	-- :: (s->a) -> Parser s t a
  , stGet	-- :: Parser s t s
    -- ** re-parsing
  , reparse	-- :: [t] -> Parser s t ()
  ) where


-- | The @Parser@ datatype is a fairly generic parsing monad with error
--   reporting and a running state.  It can be used for arbitrary token
--   types, not just String input.
newtype Parser s t a = P (s -> [t] -> (Either String a, s, [t]))

-- | Apply a parser to an initial state and input token sequence.
runParser :: Parser s t a -> s -> [t] -> (Either String a, s, [t])
runParser (P p) = p

instance Functor (Parser s t) where
    fmap f (P p) = P (\s ts-> case p s ts of
                                (Left msg, s, ts') -> (Left msg,    s, ts')
                                (Right x,  s, ts') -> (Right (f x), s, ts'))
instance Monad (Parser s t) where
    return x     = P (\s ts-> (Right x, s, ts))
    (P f) >>= g  = P (\s ts-> case f s ts of
                                (Left msg, s', ts') -> (Left msg, s', ts')
                                (Right x,  s', ts') -> let (P g') = g x
                                                       in g' s' ts')
    fail e       = P (\s ts-> (Left e, s, ts))


-- Combinators

-- | One token
next :: Parser s t t
next = P (\s ts-> case ts of
                    []  -> (Left "Ran out of input (EOF)", s, [])
                    (t:ts') -> (Right t, s, ts') )

-- | One token satifying a predicate
satisfy :: (t->Bool) -> Parser s t t
satisfy p = do{ x <- next
              ; if p x then return x else fail "Parse.satisfy: failed"
              }

infixl 3 `apply`
-- | Apply a parsed function to a parsed value
apply :: Parser s t (a->b) -> Parser s t a -> Parser s t b
pf `apply` px = do { f <- pf; x <- px; return (f x) }

infixl 3 `discard`
-- | @x `discard` y@ parses both x and y, but discards the result of y
discard :: Parser s t a -> Parser s t b -> Parser s t a
px `discard` py = do { x <- px; _ <- py; return x }

-- | @p `adjustErr` f@ applies the transformation @f@ to any error message
--   generated in @p@, having no effect if @p@ succeeds.
adjustErr :: Parser s t a -> (String->String) -> Parser s t a
(P p) `adjustErr` f = P (\s ts-> case p s ts of
                                   (Left msg, s', ts') -> (Left (f msg), s, ts')
                                   right               -> right )

infixl 6 `onFail`	-- not sure about precedence 6?
-- | @p `onFail` q@ means parse p unless p fails in which case parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
onFail :: Parser s t a -> Parser s t a -> Parser s t a
(P p) `onFail` (P q) = P (\s ts-> case p s ts of
                                    (Left _, _, _) -> q s ts
                                    right          -> right )

-- | Parse the first alternative in the list that succeeds.
oneOf :: [Parser s t a] -> Parser s t a
oneOf []     = fail ("Failed to parse any of the possible choices")
oneOf (p:ps) = p `onFail` oneOf ps

-- | 'many p' parses a list of elements with individual parser p.
--   Cannot fail, since an empty list is a valid return value.
many :: Parser s t a -> Parser s t [a]
many p = many1 p `onFail` return []

-- | Parse a non-empty list of items.
many1 :: Parser s t a -> Parser s t [a]
many1 p = do { x <- p
             ; xs <- many p
             ; return (x:xs)
             }
         `adjustErr` ("When looking for a non-empty sequence:\n\t"++)

-- | Parse a list of items separated by discarded junk.
sepBy :: Parser s t a -> Parser s t sep -> Parser s t [a]
sepBy p sep = do sepBy1 p sep `onFail` return []

-- | Parse a non-empty list of items separated by discarded junk.
sepBy1 :: Parser s t a -> Parser s t sep -> Parser s t [a]
sepBy1 p sep = do { x <- p
                  ; xs <- many (do {sep; p})
                  ; return (x:xs)
                  }
         `adjustErr` ("When looking for a non-empty sequence:\n\t"++)
 
-- | Parse a non-empty list of items, discarding the start, end, and separator
--   items.
bracketSep :: Parser s t bra -> Parser s t sep -> Parser s t ket
              -> Parser s t a -> Parser s t [a]
bracketSep open sep close p =
    do { open    `adjustErr` ("Missing opening bracket:\n\t"++)
       ; x <- p  `adjustErr` ("After first bracket in a group:\n\t"++)
       ; xs <- many (do {sep; p})
       ; close   `adjustErr` ("When looking for closing bracket:\n\t"++)
       ; return (x:xs)
       }

-- | Parse a bracketed item, discarding the brackets.
bracket :: Parser s t bra -> Parser s t ket -> Parser s t a -> Parser s t a
bracket open close p = do
    do { open    `adjustErr` ("Missing opening bracket:\n\t"++)
       ; x <- p
       ; close   `adjustErr` ("Missing closing bracket:\n\t"++)
       ; return x
       }

------------------------------------------------------------------------
-- State handling

-- | Update the internal state.
stUpdate   :: (s->s) -> Parser s t ()
stUpdate f  = P (\s ts-> (Right (), f s, ts))

-- | Query the internal state.
stQuery    :: (s->a) -> Parser s t a
stQuery f   = P (\s ts-> (Right (f s), s, ts))

-- | Deliver the entire internal state.
stGet      :: Parser s t s
stGet       = P (\s ts-> (Right s, s, ts))

------------------------------------------------------------------------
-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [t] -> Parser s t ()
reparse ts  = P (\s inp-> (Right (), s, ts++inp))

------------------------------------------------------------------------
