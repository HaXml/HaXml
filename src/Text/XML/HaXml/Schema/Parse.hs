module Text.XML.HaXml.Schema.Parse
  ( module Text.XML.HaXml.Schema.Parse
  ) where

import Char (isSpace)
import List (intersperse)
import Text.ParserCombinators.Poly
import Text.Parse    -- for String parsers

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Verbatim
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Schema.XSDTypeModel as XSD

xsd :: Name -> QName
xsd name = QN nullNamespace{nsURI="http://www.w3.org/2001/XMLSchema"}
              name

-- | We need a Parser monad for reading from a sequence of generic XML
--   Contents into specific datatypes that model the structure of XSD
--   descriptions.  This is a specialisation of the polyparse combinators,
--   fixing the input token type.
type XsdParser a = Parser (Content Posn) a

-- | The most primitive combinator for XsdParser - get one content item.
content :: String -> XsdParser (Content Posn)
content word = next `adjustErr` (++" when expecting "++word)

-- | Get the next content element, checking that it has one of the required
--   tags, using the given matching function.
--   (Skips over comments and whitespace, rejects text and refs.
--    Also returns position of element.)
posnElementWith :: (QName->Bool) -> [String] -> XsdParser (Posn,Element Posn)
posnElementWith match tags = do
    { c <- content (formatted tags)
    ; case c of
        CElem e@(Elem t _ _) pos
            | match t   -> return (pos,e)
            | otherwise -> fail ("Found a <"++printableName t
                                 ++">, but expected "
                                 ++formatted tags++"\nat "++show pos)
        CString b s pos  -- ignore blank space
            | not b && all isSpace s -> posnElementWith match tags
            | otherwise -> fail ("Found text content, but expected "
                                 ++formatted tags++"\ntext is: "++s
                                 ++"\nat "++show pos)
        CRef r pos -> fail ("Found reference, but expected "
                            ++formatted tags++"\nreference is: "++verbatim r
                            ++"\nat "++show pos)
        CMisc _ _ -> posnElementWith match tags  -- skip comments, PIs, etc.
    }
  where
    formatted [t]  = "a <"++t++">"
    formatted tgs = "one of"++ concatMap (\t->" <"++t++">") tgs

-- | Get the next content element, checking that it has the required QName.
element :: QName -> XsdParser (Element Posn)
element qn = fmap snd (posnElementWith (==qn) (printableName qn:[]))

-- | Run an XsdParser on the contents of the given element (i.e. not on the
--   current monadic content sequence), checking that the contents are
--   exhausted, before returning the calculated value within the current
--   parser context.
interior :: Element Posn -> XsdParser a -> XsdParser a
interior = interiorWith (const True)
{-
interior (Elem e _ cs) p =
    case runParser p cs of
        (Left msg, _) -> fail msg
        (Right x, []) -> return x
        (Right x, ds@(d:_))
            | all onlyMisc ds -> return x
            | otherwise       -> fail ("Too many elements inside <"
                                      ++printableName e++"> at\n"
                                      ++show (info d)++"\n"
                                      ++"Found excess: "++verbatim d)
  where onlyMisc (CMisc _ _) = True
        onlyMisc (CString False s _) | all isSpace s = True
        onlyMisc _ = False
-}

-- | Like interior, only we filter the child content before using it.
interiorWith :: (Content Posn->Bool) -> Element Posn
                -> XsdParser a -> XsdParser a
interiorWith keep (Elem e _ cs) p =
    case runParser p (filter keep cs) of
        (Left msg, _) -> fail msg
        (Right x, []) -> return x
        (Right x, ds@(d:_))
            | all onlyMisc ds -> return x
            | otherwise       -> fail ("Too many elements inside <"
                                      ++printableName e++"> at\n"
                                      ++show (info d)++"\n"
                                      ++"Found excess: "++verbatim d)
  where onlyMisc (CMisc _ _) = True
        onlyMisc (CString False s _) | all isSpace s = True
        onlyMisc _ = False

-- | Check for the presence (and value) of an attribute in the given element.
--   Absence results in failure.
attribute :: QName -> TextParser a -> Element Posn -> XsdParser a
attribute qn p (Elem _ as _) =
    case lookup qn as of
        Nothing  -> fail $ "attribute "++printableName qn++" not present"
        Just atv -> case runParser p (show atv) of
                      (Left msg,_) -> fail $ "Attribute parsing failure: "
                                             ++printableName qn++"=\""
                                             ++show atv++"\": "++msg
                      (Right v,[]) -> return v
                      (Right v,xs) -> fail $ "Attribute parsing excess text: "
                                             ++printableName qn++"=\""
                                             ++show atv++"\":\n  Excess is: "
                                             ++xs

-- Now for the real parsers.


-- | Parse a Schema declaration
schema :: XsdParser Schema
schema = do
    e <- element (xsd "schema") `adjustErr`  (++"Expected <xsd:schema>")
    attrQualDef <- attribute (xsd "attributeFormDefault") qform e
                   `onFail` return Unqualified
    elemQualDef <- attribute (xsd "elementFormDefault") qform e
                   `onFail` return Unqualified
    target      <- optional (attribute (N "targetNamespace") word e)
    annote      <- interiorWith annTag       e (optional annotation)
    items       <- interiorWith (not.annTag) e (many schemaItem)
    return $ Schema items (maybe NoAnnotation id annote)
                    attrQualDef elemQualDef Nothing target Nothing
    -- Note: does not yet deal with Final or Block attributes.
  where
    annTag (CElem (Elem qn _ _) _) = qn == xsd "annotation"
    annTag _                       = False

-- | Just a dummy for now.
schemaItem :: XsdParser SchemaItem
schemaItem = do
    _ <- content "to make progress"
    return $ Simple (Primitive String) NoAnnotation

-- | Just a dummy for now.
annotation :: XsdParser Annotation
annotation = do
    return $ NoAnnotation

qform :: TextParser QForm
qform = do w <- word
           case w of
             "qualified"   -> return Qualified
             "unqualified" -> return Unqualified
             _             -> failBad "Expected \"qualified\" or \"unqualified\""


