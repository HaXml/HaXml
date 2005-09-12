-- | The class 'Haskell2XmlNew' is a replacement for Read and Show: it provides
--   textual conversions (to and from an XML representation) for your
--   Haskell data values.  Use the tool
--   DrIFT to derive this class for your own datatypes, then
--   include this module where you want to use the facilities.
--
--   The methods 'toContents' and 'fromContents' convert a value to and from
--   a generic internal representation of an XML document /without/ a DTD.
--   The functions 'toXml' and 'fromXml' convert a value to and from a generic
--   internal representation of an XML document /including/ a DTD.
--   The functions 'readXml' and 'showXml' convert to and from Strings.
--   The functions 'fReadXml' and 'fWriteXml' do the conversion to and from
--   the given filenames.
--   The functions 'hGetXml' and 'hPutXml' do the conversion to and from
--   the given file handles.
--   (See the type signatures.)

-- The "New" in the name refers to a significant change of API over
-- the original Haskell2Xml class.

module Text.XML.HaXml.Haskell2XmlNew
  ( -- * Re-export the entire set of XML type definitions
    module Text.XML.HaXml.Types
  -- * The class Haskell2Xml
  , Haskell2XmlNew(..)
  -- ** auxiliaries for writing member functions
  , interior, element
  -- ** Conversion functions
  , toXml, toDTD, fromXml
  , readXml, showXml
  -- ** IO conversion functions
  , fReadXml, fWriteXml
  , hGetXml,  hPutXml
  -- * Auxiliary types
  , HType(..)
  , Constr(..)
  -- Convenience functions
  , mkElem , mkElemC
  , showConstr
  , isPrefixOf
  ) where

import IO

import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse (xmlParse)
import Text.PrettyPrint.HughesPJ (render)
import qualified Text.XML.HaXml.Pretty as PP
import List(intersperse,isPrefixOf,isSuffixOf,partition)
import Char (ord)


-- | A concrete representation of any Haskell type.
data HType =
      Maybe HType
    | List HType
    | Tuple [HType]
    | Prim String String	-- ^ separate Haskell name and Xml name
    | String
    | Defined String [HType] [Constr]
	-- ^ A user-defined type has a name, a sequence of type variables,
	--   and a set of constructors.
    deriving (Show)

instance Eq HType where
    (Maybe x)  == (Maybe y)  =  x==y
    (List x)   == (List y)   =  x==y
    (Tuple xs) == (Tuple ys) =  xs==ys
    (Prim x _) == (Prim y _) =  x==y
    String     == String     =  True
    (Defined n xs _) == (Defined m ys _)  =  n==m && xs==ys
    _          == _          =  False

-- | A concrete representation of any user-defined Haskell constructor.
--   The constructor has a name, and a sequence of component types.  The
--   first sequence of types represents the minimum set of free type
--   variables occurring in the (second) list of real component types.
--   If there are fieldnames, they are contained in the final list, and
--   correspond one-to-one with the component types.
data Constr = Constr String [HType] [HType] -- (Maybe [String])
    deriving (Eq,Show)


attval :: (Read a) => Element -> a
attval (Elem _ [("value",AttValue [Left s])] []) = read s

mkAttr :: String -> String -> Attribute
mkAttr n v = (n, AttValue [Left v])


-- | We need a parsing monad for reading generic XML Content into specific
--   datatypes.  We could try to specialise the HuttonMeijerWallace parser
--   combinators... but their internals are overly complex for this purpose.
--   Better just to roll our own.
data XMLParser a = XP ([Content] -> Either String (a,[Content]))
runXMLParser :: XMLParser a -> [Content] -> Either String (a,[Content])
runXMLParser (XP p) = (\cs -> p cs)

instance Functor XMLParser where
    fmap f (XP p) = XP (\cs-> case p cs of
                                Left msg      -> Left msg
                                Right (x,cs') -> Right (f x, cs'))
instance Monad XMLParser where
    return x      = XP (\cs-> Right (x,cs))
    (XP f) >>= g  = XP (\cs-> case f cs of
                                Left msg      -> Left msg
                                Right (x,cs') -> let (XP g') = g x in g' cs')
    fail s        = XP (\cs-> Left s)

-- | The most primitive combinator for XMLParser - get one content item.
content :: String -> XMLParser Content
content word = XP (\cs ->
    case cs of []     -> Left ("Ran out of input when expecting "++word)
               (x:xs) -> Right (x,xs))

-- | Get the next content element (ignoring comments, text, etc), and check
--   that it has one of the required tag prefixes.
element :: [String] -> XMLParser Element
element tags = do
    { c <- content (formatted tags)
    ; case c of
          CElem e@(Elem t _ _)
              | any (`isPrefixOf` t) tags -> return e
              | otherwise -> fail ("Found a <"++t++">, but expected "
                                  ++formatted tags)
          _ -> element tags	-- skip XML text, comments, etc
    }
  where
    formatted [t]  = "a <"++t++">"
    formatted tags = "one of"++ concatMap (\t->" <"++t++">") tags
                                 
-- | Run an XMLParser on the contents of the given element (i.e. not on the
--   current monadic content sequence), checking that the contents are
--   exhausted, before returning the calculated value within the current
--   parser context.
interior :: Element -> XMLParser a -> XMLParser a
interior (Elem e _ cs) p = do
    { case runXMLParser p cs of
          Left msg     -> fail msg
          Right (x,[]) -> return x
          Right (x,_)  -> fail ("Too many elements inside <"++e++">")
    }


-- | A class to convert any Haskell value to and from an XML representation.
class Haskell2XmlNew a where
    -- | Determine the type of the Haskell value (to create a DTD).
    toHType      :: a -> HType
    -- | Convert the Haskell value to a generic XML value.
    toContents   :: a -> [Content]
    -- | Parse a Haskell value from a generic XML representation, returning
    --   the value and the remainder of the XML.  (Or an error message.)
    fromContents :: [Content] -> Either String (a,[Content])
    parseContents :: XMLParser a 

    --   Only one of 'fromContents' or 'parseContents' need be defined.
    fromContents  = runXMLParser parseContents
    parseContents = XP (\cs-> fromContents cs)

    -- | This function is a dummy for most types: it is used /only/ in
    --   the Char instance for coercing lists of Char into String.
    xToChar      :: a -> Char
    xToChar       = error "Haskell2Xml.xToChar used in error"
    -- | This function is a dummy for most types: it is used /only/ in
    --   the Char instance for coercing lists of Char into String.
    xFromChar    :: Char -> a
    xFromChar     = error "Haskell2Xml.xFromChar used in error"

instance Haskell2XmlNew Bool where
    toHType   _    = Prim "Bool" "bool"
    toContents b   = [CElem (Elem "bool" [mkAttr "value" (show b)] [])]
    parseContents = do { e <- element ["bool"] ; return (attval e) }

instance Haskell2XmlNew Int where
    toHType   _    = Prim "Int" "int"
    toContents i   = [CElem (Elem "int" [mkAttr "value" (show i)] [])]
    parseContents = do { e <- element ["int"] ; return (attval e) }

instance Haskell2XmlNew Integer where
    toHType   _    = Prim "Integer" "integer"
    toContents i   = [CElem (Elem "integer" [mkAttr "value" (show i)] [])]
    parseContents = do { e <- element ["integer"] ; return (attval e) }

instance Haskell2XmlNew Float where
    toHType   _    = Prim "Float" "float"
    toContents i   = [CElem (Elem "float" [mkAttr "value" (show i)] [])]
    parseContents = do { e <- element ["float"] ; return (attval e) }

instance Haskell2XmlNew Double where
    toHType   _    = Prim "Double" "double"
    toContents i   = [CElem (Elem "double" [mkAttr "value" (show i)] [])]
    parseContents = do { e <- element ["float"] ; return (attval e) }

instance Haskell2XmlNew Char where
    -- NOT in a string
    toHType   _    = Prim "Char" "char"
    toContents c   = [CElem (Elem "char" [mkAttr "value" [c]] [])]
    parseContents = do { (Elem _ [("value",(AttValue [Left [c]]))] [])
                             <- element ["char"]
                       ; return c
                       }

    -- Only defined for Char and no other types:
    xToChar   = id
    xFromChar = id

instance Haskell2XmlNew a => Haskell2XmlNew [a] where
    toHType xs     = case toHType x of
                       (Prim "Char" _) -> String
                       _ -> List (toHType x)
                   where   (x:_) = xs
    toContents xs  = case toHType x of
                       (Prim "Char" _) ->
                            [mkElem "string" [CString False (map xToChar xs)]]
                       _ -> [mkElem xs (concatMap toContents xs)]
                   where   (x:_) = xs
    fromContents (CString _ s:cs)
                   = Right (map xFromChar s,cs)
    fromContents (CElem (Elem "string" [] [CString _ s]):cs)
                   = Right (map xFromChar s,cs) --fromContents cs 
    fromContents (CElem (Elem e [] xs):cs) | "list" `isPrefixOf` e
                   = scanElements xs
                   where
                  -- scanElements :: [Content] -> Either String ([a],[Content])
                     scanElements [] = Right ([], cs)
                     scanElements es =
                        case fromContents es of
                            Left msg -> Left msg
                            Right (x,es') ->
                                case scanElements es' of
                                    Left msg -> Left msg
                                    Right (xs,cs) -> Right (x:xs, cs)
    fromContents (CElem (Elem e _ _): cs)
                   = Left ("Expected a <list-...>, but found a <"++e++">")
    fromContents (_:cs) = fromContents cs	-- skip comments etc.

instance (Haskell2XmlNew a, Haskell2XmlNew b) => Haskell2XmlNew (a,b) where
    toHType p        = Tuple [toHType a, toHType b]   where   (a,b) = p
    toContents (a,b) = toContents a ++ toContents b
    parseContents    = do { a <- parseContents
                          ; b <- parseContents
                          ; return (a,b)
                          }

instance (Haskell2XmlNew a) => Haskell2XmlNew (Maybe a) where
    toHType m      = Maybe (toHType x)   where   (Just x) = m
    toContents m   = [mkElem m (maybe [] toContents m)]
    parseContents = do
        { e <- element ["maybe"]
        ; case e of (Elem _ [] []) -> return Nothing
                    (Elem _ [] _)  -> fmap Just (interior e parseContents)
        }

instance (Haskell2XmlNew a, Haskell2XmlNew b) => Haskell2XmlNew (Either a b) where
    toHType m  = Defined "Either" [hx, hy]
                         [Constr "Left" [hx] [hx] {-Nothing-}
                         ,Constr "Right" [hy] [hy] {-Nothing-}]
               where   (Left x)  = m
                       (Right y) = m
                       hx = toHType x
                       hy = toHType y
    toContents v@(Left aa) =
        [mkElemC (showConstr 0 (toHType v)) (toContents aa)]
    toContents v@(Right ab) =
        [mkElemC (showConstr 1 (toHType v)) (toContents ab)]
    parseContents = do
        { e@(Elem t [] _) <- element ["Left","Right"]
        ; case t of
            _ | "Left"  `isPrefixOf` t -> fmap Left  (interior e parseContents)
              | "Right" `isPrefixOf` t -> fmap Right (interior e parseContents)
        }

instance Haskell2XmlNew () where
    toHType _      = Prim "unit" "unit"
    toContents ()  = [CElem (Elem "unit" [] [])]
    parseContents = do { element ["unit"]; return () }


mkElem x cs  = CElem (Elem (flat (toHType x) "") [] cs)
mkElemC x cs = CElem (Elem x [] cs)


-- | 'toDTD' converts a concrete representation of the Haskell type of
--   a value (obtained by the method 'toHType') into a real DocTypeDecl.
--   It ensures that PERefs are defined before they are used, and that no
--   element or attribute-list is declared more than once.
toDTD :: HType -> DocTypeDecl
toDTD ht =
  DTD (toplevel ht) Nothing (macrosFirst (reverse (h2d True [] [] [ht])))
  where
    macrosFirst :: [MarkupDecl] -> [MarkupDecl]
    macrosFirst decls = concat [p, p'] where (p, p') = partition f decls
                                             f (Entity _) = True
                                             f _ = False
    toplevel ht@(Defined _ _ _) = flat ht "-XML"
    toplevel ht@_               = flat ht ""
    c0 = False
    h2d :: Bool -> [HType] -> [Constr] -> [HType] -> [MarkupDecl]
    -- toplevel?   history    history   remainingwork     result
    h2d c history chist []       = []
    h2d c history chist (ht:hts) =
      if ht `elem` history then h2d c0 history chist hts
      else
        case ht of
          Maybe ht0  -> declelem ht: h2d c0 (ht:history) chist (ht0:hts)
          List ht0   -> declelem ht: h2d c0 (ht:history) chist (ht0:hts)
          Tuple hts0 -> (c ? (declelem ht:))
                                     (h2d c0 history chist (hts0++hts))
          Prim s t   -> declprim ht ++ h2d c0 (ht:history) chist hts
          String     -> declstring:    h2d c0 (ht:history) chist hts
          Defined s _ cs ->
               let hts0 = concatMap grab cs in
               (c ? (decltopelem ht:)) (declmacro ht chist)
               ++ h2d c0 (ht:history) (cs++chist) (hts0++hts)
    declelem ht =
      Element (ElementDecl (flat ht "") (ContentSpec (outerHtExpr ht)))
    decltopelem ht =	-- hack to avoid peref at toplevel
      Element (ElementDecl (flat ht "-XML") (ContentSpec (innerHtExpr ht None)))
    declmacro ht@(Defined _ _ cs) chist =
      Entity (EntityPEDecl (PEDecl (flat ht "") (PEDefEntityValue ev))):
      concatMap (declConstr chist) cs
      where ev = EntityValue [EVString (render (PP.cp (outerHtExpr ht)))]
    declConstr chist c@(Constr s fv hts {-_-})
      | c `notElem` chist =
          [Element (ElementDecl (cflat c "") (ContentSpec (constrHtExpr c)))]
      | otherwise = []
    declprim (Prim s t) =
      [ Element (ElementDecl t EMPTY)
      , AttList (AttListDecl t [AttDef "value" StringType REQUIRED])]
    declstring =
      Element (ElementDecl "string" (Mixed PCDATA))
    grab (Constr _ _ hts {-_-}) = hts

(?) :: Bool -> (a->a) -> (a->a)
b ? f | b     = f
      | not b = id


flat :: HType -> ShowS
flat (Maybe ht)       = showString "maybe-" . flat ht
flat (List ht)        = showString "list-" . flat ht
flat (Tuple hts)      = showString "tuple" . shows (length hts) .
                        showChar '-' .
                        foldr1 (.) (intersperse (showChar '-') (map flat hts))
flat (Prim s t)       = showString t
flat String           = showString "string"
flat (Defined s fv _) = showString s . ((length fv > 0) ? (showChar '-')) .
                        foldr (.) id (intersperse (showChar '-') (map flat fv))
cflat :: Constr -> ShowS
cflat (Constr s fv _ {-_-}) = showString s . ((length fv > 0) ? (showChar '-')) .
                        foldr (.) id (intersperse (showChar '-') (map flat fv))

outerHtExpr :: HType -> CP
outerHtExpr (Maybe ht)      = innerHtExpr ht Query
outerHtExpr (List ht)       = innerHtExpr ht Star
outerHtExpr (Defined s fv cs) =
    Choice (map (\c->TagName (cflat c "") None) cs) None
outerHtExpr ht              = innerHtExpr ht None

innerHtExpr :: HType -> Modifier -> CP
innerHtExpr (Prim s t)  m = TagName t m
innerHtExpr (Tuple hts) m = Seq (map (\c-> innerHtExpr c None) hts) m
innerHtExpr ht@(Defined s hts cs) m = -- CPPE (flat ht "") (outerHtExpr ht)
                                      TagName ('%': flat ht ";") m
							--  ***HACK!!!***
innerHtExpr ht m = TagName (flat ht "") m

constrHtExpr (Constr s fv [] {-_-})  = TagName "EMPTY" None	--  ***HACK!!!***
constrHtExpr (Constr s fv hts {-_-}) = innerHtExpr (Tuple hts) None



---------------------------
-- Exported user functions.
---------------------------

-- | Convert any Haskell value to an XML document, including both DTD and
--   content.
toXml :: Haskell2XmlNew a => a -> Document
toXml value =
  let ht = toHType value in
  Document (Prolog Nothing [] (Just (toDTD ht)) [])
           emptyST
           (case (ht, toContents value) of
             (Tuple _, cs) -> (Elem (flat ht "") [] cs)
             (Defined _ _ _, cs) -> (Elem (flat ht "-XML") [] cs)
             (_,[CElem e]) -> e )
           []

-- | Read a Haskell value from an XML document, ignoring the DTD and
--   using the Haskell result type to determine how to parse it.
fromXml :: Haskell2XmlNew a => Document -> Either String a
fromXml (Document _ _ e@(Elem n _ cs) _)
  | "tuple" `isPrefixOf` n = dropSnd (fromContents cs)
  | "-XML"  `isSuffixOf` n = dropSnd (fromContents cs)
  | otherwise = dropSnd (fromContents [CElem e])
  where dropSnd (Right (x,_)) = Right x
        dropSnd (Left msg)    = Left msg

-- | Convert an XML document encoded as a String, into a Haskell value.
readXml :: Haskell2XmlNew a => String -> Either String a
readXml = fromXml . xmlParse "string input"
-- | Convert a Haskell value to an XML document, encoded as a String.
showXml :: Haskell2XmlNew a => a -> String
showXml = render . PP.document . toXml


-- | Read a Haskell value from an XML document stored in a file.
fReadXml  :: Haskell2XmlNew a => FilePath -> IO a
fReadXml fp = do
    f <- openFile fp ReadMode 
    content <- hGetContents f
    --hClose f
    either (ioError.userError) return (fromXml (xmlParse fp content))

-- | Write a Haskell value to the given file as an XML document.
fWriteXml :: Haskell2XmlNew a => FilePath -> a -> IO ()
fWriteXml fp v = do
    f <- openFile fp WriteMode 
    (hPutStrLn f . render . PP.document . toXml) v
    hClose f

-- | Read a Haskell value from an XML document transmitted through the
--   given 'Handle'.
hGetXml  :: Haskell2XmlNew a => Handle -> IO a
hGetXml f = do
    content <- hGetContents f
    either (ioError.userError) return (fromXml (xmlParse "file handle" content))

-- | Write a Haskell value to the given 'Handle' as an XML document.
hPutXml :: Haskell2XmlNew a => Handle -> a -> IO ()
hPutXml f v = (hPutStrLn f . render . PP.document . toXml) v


showConstr n (Defined _ _ cs) = cflat (cs!!n) ""
showConstr n _ = error "no constructors for builtin types"

-- END
