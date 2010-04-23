module Text.XML.HaXml.Schema.Parse
  ( module Text.XML.HaXml.Schema.Parse
  ) where

import Char (isSpace)
-- import Text.ParserCombinators.Poly
import Text.Parse    -- for String parsers

import Text.XML.HaXml.Types      (Name,QName(..),Namespace(..)
                                 ,Content(..),Element(..),info)
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Verbatim hiding (qname)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Schema.XSDTypeModel as XSD
import Text.XML.HaXml.XmlContent.Parser (text)


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
content msg = next `adjustErr` (++" when expecting "++msg)

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
                                      ++"Found excess: "++verbatim (take 5 ds))
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
                      (Right _,xs) -> fail $ "Attribute parsing excess text: "
                                             ++printableName qn++"=\""
                                             ++show atv++"\":\n  Excess is: "
                                             ++xs

-- Now for the real parsers.


-- | Parse a Schema declaration
schema :: XsdParser Schema
schema = do
    e <- element (xsd "schema") `adjustErr`  (++"Expected <xsd:schema>")
    attrQualD <- attribute (N "attributeFormDefault") qform e
                 `onFail` return Unqualified
    elemQualD <- attribute (N "elementFormDefault") qform e
                 `onFail` return Unqualified
    finalD    <- optional (attribute (xsd "finalDefault") final e)
    blockD    <- optional (attribute (xsd "blockDefault") final e)
    target    <- optional (attribute (N "targetNamespace") uri  e)
    annote    <- interiorWith (xsdTag "annotation")     e annotation
    items     <- interiorWith (not.xsdTag "annotation") e (many schemaItem)
    return $ Schema annote attrQualD elemQualD finalD blockD target items

-- | Predicate for comparing against an XSD-qualified name
xsdTag :: String -> Content Posn -> Bool
xsdTag tag (CElem (Elem qn _ _) _)  =  qn == xsd tag
xsdTag _   _                        =  False

-- | Parse an <xsd:annotation> element.
annotation :: XsdParser Annotation
annotation = do
    me <- optional (element (xsd "annotation"))
    case me of
      Nothing -> return (NoAnnotation "missing")
      Just e -> fmap Documentation
                             (interiorWith (xsdTag "documentation") e allText)
                    `onFail`
                fmap AppInfo (interiorWith (xsdTag "appinfo") e allText)
                    `onFail`
                return (NoAnnotation "failed to parse")
  where
    allText = do (_,e) <- posnElementWith (const True) ["anything"]
                 interiorWith (const True) e text


-- | Parse a FormDefault attribute.
qform :: TextParser QForm
qform = do
    w <- word
    case w of
        "qualified"   -> return Qualified
        "unqualified" -> return Unqualified
        _             -> failBad "Expected \"qualified\" or \"unqualified\""

-- | Parse a Final or Block attribute.
final :: TextParser Final
final = do
    w <- word
    case w of
        "restriction" -> return NoRestriction
        "extension"   -> return NoExtension
        "#all"        -> return AllFinal
        _             -> failBad $ "Expected \"restriction\" or \"extension\""
                                   ++" or \"#all\""

-- | Parse a schema item (just under the toplevel <xsd:schema>)
schemaItem :: XsdParser SchemaItem
schemaItem = oneOf'
       [ extractAnnotation (xsd "element")     Decl    elementDecl
       , extractAnnotation (xsd "simpleType")  Simple  simpleType
       , extractAnnotation (xsd "complexType") (\c a-> Complex c a Nothing)
                                                       complexType
       , ("xsd:include", include)
    -- , lots more required:
    --    Redefine
    --    Substitution
    --    Abstract
    --    Constraint
    --    Import
       , extractAnnotation (xsd "group") ($) groupDecl
       ]

-- | Auxiliary to help lift a parsed annotation out of a parsed element.
extractAnnotation :: QName -> (a->Annotation->b) -> XsdParser a
                     -> (String, XsdParser b)
extractAnnotation qn build parser =
    ( printableName qn
    , do (p,e) <- posnElementWith (==qn) (printableName qn:[])
         reparse [CElem e p]
         v <- parser
         annote <- interiorWith (xsdTag "annotation") e annotation
         return $ build v annote
    )

-- | Parse an <xsd:include>.
include :: XsdParser SchemaItem
include = do e <- element (xsd "include")
             v <- attribute (N "schemaLocation") uri e
             return (Include v)

-- | Parse an <xsd:element> decl.
elementDecl :: XsdParser ElementDecl
elementDecl =
      ( do e    <- element (xsd "element")
           n    <- attribute (N "name") name e
           t    <- attribute (N "type") (fmap Left qname) e
                   `onFail`
                   interiorWith (\c-> xsdTag "simpleType" c ||
                                      xsdTag "complexType" c)
                                e
                                (fmap (Right . Left) simpleType
                                 `onFail`
                                 fmap (Right . Right) complexType)
           min  <- optional $ attribute (N "minOccurs") parseDec e
           max  <- optional $ attribute (N "maxOccurs") parseDec e
           defV <- attribute (N "default") string e `onFail` return ""
           nil  <- attribute (N "nillable") bool e `onFail` return False
           qf   <- attribute (N "form") qform e `onFail` return Unqualified
           return (ElementDecl n t (Occurs min max) defV nil qf)
      ) `onFail` (
        do e    <- element (xsd "element")
           ref  <- attribute (N "ref") qname e
           min  <- optional $ attribute (N "minOccurs") parseDec e
           max  <- optional $ attribute (N "maxOccurs") parseDec e
           return (ElementRef ref (Occurs min max))
      ) -- and two other cases, for GroupRef and Choice.

-- | Parse an <xsd:attribute> decl.
attributeDecl :: XsdParser AttributeDecl
attributeDecl =
      ( do e  <- element (xsd "attribute")
           n  <- attribute (N "name") name e
           t  <- attribute (N "type") (fmap Left qname) e
                 `onFail`
                 interiorWith (xsdTag "simpleType") e (fmap Right simpleType)
           u  <- attribute (N "use") use e
                 `onFail` return Optional
           df <- optional (attribute (N "default") (fmap Left string) e
                           `onFail`
                           attribute (N "fixed") (fmap Right string) e)
           a  <- interiorWith (xsdTag "annotation") e annotation
           qf <- attribute (N "form") qform e `onFail` return Unqualified
           return (AttributeDecl n t u df a qf)
      ) `onFail` (
        do e   <- element (xsd "attribute")
           ref <- attribute (N "ref") qname e
           return (AttributeGroupRef ref)
      )

-- | Parse a <xsd:group> decl.
groupDecl :: XsdParser (Annotation->SchemaItem)
groupDecl = do e <- element (xsd "group")
               commit $ do n <- attribute (N "name") string e
                           -- dummy elementdecl content
                           return (Group n [])

-- | Parse a <xsd:simpleType> decl.
simpleType :: XsdParser SimpleType
simpleType = do e <- element (xsd "simpleType")
                n <- optional (attribute (N "name") (fmap N name) e)
                case n of
                  Nothing -> return (Primitive String) -- dummy for now
                  Just _  -> return (Primitive String) -- also a dummy

-- | Parse a <xsd:complexType> decl.
complexType :: XsdParser ComplexType
complexType = do e <- element (xsd "complexType")
                 n <- optional (attribute (N "name") name e)
                 -- just a dummy for now
                 return (ComplexType n [] (Sequence []) False Nothing)
         --   interiorWith (xsd "simpleContent") e

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Text parser for a URI (very simple, non-validating, probably incorrect).
uri :: TextParser String
uri = string

-- | Text parser for an arbitrary string consisting of possibly multiple tokens.
string :: TextParser String
string = fmap concat $ many word

-- | Parse a textual boolean, i.e. "true", "false", "0", or "1"
bool :: TextParser Bool
bool = do w <- word
          case w of
            "true"  -> return True
            "false" -> return False
            "0"     -> return True
            "1"     -> return False
            _       -> fail "could not parse boolean value"

-- | Parse a "use" attribute value, i.e. "required", "optional", or "prohibited"
use :: TextParser Use
use = do w <- word
         case w of
           "required"   -> return Required
           "optional"   -> return Optional
           "prohibited" -> return Prohibited
           _            -> fail "could not parse \"use\" attribute value"

-- | Parse an attribute value that should be a QName.
qname :: TextParser QName
qname = do a <- word
           ( do ":" <- word
                b   <- word
                return (N (a++':':b))
             `onFail`
             do eof
                return (N a) )

-- | Parse an attribute value that should be a simple Name.
name :: TextParser Name
name = word
