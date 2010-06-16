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


-- | Lift boolean 'or' over predicates.
(|||) :: (a->Bool) -> (a->Bool) -> (a->Bool)
p ||| q = \v -> p v || q v

-- | Qualify an ordinary name with the XSD namespace.
xsd :: Name -> QName
xsd name = QN Namespace{nsPrefix="XSD",nsURI="http://www.w3.org/2001/XMLSchema"}
              name

-- | Predicate for comparing against an XSD-qualified name
xsdTag :: String -> Content Posn -> Bool
xsdTag tag (CElem (Elem qn _ _) _)  =  qn == xsd tag
xsdTag _   _                        =  False

-- | We need a Parser monad for reading from a sequence of generic XML
--   Contents into specific datatypes that model the structure of XSD
--   descriptions.  This is a specialisation of the polyparse combinators,
--   fixing the input token type.
type XsdParser a = Parser (Content Posn) a

-- | Get the next content element, checking that it matches some criterion
--   given by the predicate.
--   (Skips over comments and whitespace, rejects text and refs.
--    Also returns position of element.)
--   The list of strings argument is for error reporting - it usually
--   represents a list of expected tags.
posnElementWith :: (Content Posn->Bool) -> [String]
                   -> XsdParser (Posn,Element Posn)
posnElementWith match tags = do
    { c <- next `adjustErr` (++" when expecting "++formatted tags)
    ; case c of
        CElem e pos
            | match c   -> return (pos,e)
        CElem (Elem t _ _) pos
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

-- | Get the next content element, checking that it has the required tag
--   belonging to the XSD namespace.
xsdElement :: Name -> XsdParser (Element Posn)
xsdElement n = fmap snd (posnElementWith (xsdTag n) [n])

-- | Get the next content element, whatever it is.
anyElement :: XsdParser (Element Posn)
anyElement = fmap snd (posnElementWith (const True) ["any element"])

-- | Grab and parse any and all children of the next element.
allChildren :: XsdParser a -> XsdParser a
allChildren p = do e <- anyElement
                   interiorWith (const True) p e

-- | Run an XsdParser on the child contents of the given element (i.e. not
--   in the current monadic content sequence), filtering the children
--   before parsing, and checking that the contents are exhausted, before
--   returning the calculated value within the current parser context.
interiorWith :: (Content Posn->Bool) -> XsdParser a
                -> Element Posn -> XsdParser a
interiorWith keep (P p) (Elem e _ cs) = P $ \inp->
    tidy inp $
    case p (filter keep cs) of
        Committed r        -> r
        f@(Failure _ _)    -> f
        s@(Success [] _)   -> s
        Success ds@(d:_) a
            | all onlyMisc ds -> Success [] a
            | otherwise       -> Committed $
                                 Failure ds ("Too many elements inside <"
                                             ++printableName e++"> at\n"
                                             ++show (info d)++"\n\n"
                                             ++"Found excess: "
                                             ++verbatim (take 5 ds))
  where onlyMisc (CMisc _ _) = True
        onlyMisc (CString False s _) | all isSpace s = True
        onlyMisc _ = False

-- | Check for the presence (and value) of an attribute in the given element.
--   Absence results in failure.
attribute :: QName -> TextParser a -> Element Posn -> XsdParser a
attribute qn (P p) (Elem n as _) = P $ \inp->
    case lookup qn as of
        Nothing  -> Failure inp $ "attribute "++printableName qn
                                  ++" not present in <"++printableName n++">"
        Just atv -> tidy inp $
                    case p (show atv) of
                      Committed r   -> r
                      Failure z msg -> Failure z $
                                             "Attribute parsing failure: "
                                             ++printableName qn++"=\""
                                             ++show atv++"\": "++msg
                      Success [] v  -> Success [] v
                      Success xs _  -> Committed $ 
                                       Failure xs $
                                             "Attribute parsing excess text: "
                                             ++printableName qn++"=\""
                                             ++show atv++"\":\n  Excess is: "
                                             ++xs

-- | Tidy up the parsing context.
tidy :: t -> Result x a -> Result t a
tidy inp (Committed r) = tidy inp r
tidy inp (Failure _ m) = Failure inp m
tidy inp (Success _ v) = Success inp v


-- Now for the real parsers.


-- | Parse a Schema declaration
schema = do
    e <- xsdElement "schema" `adjustErr` (++"Expected <xsd:schema>")
    commit $ return Schema
    --   `apply` interiorWith (xsdTag "annotation")     annotation e
         `apply` (attribute (N "elementFormDefault")    qform e
                  `onFail` return Unqualified)
         `apply` (attribute (N "attributeFormDefault")  qform e
                  `onFail` return Unqualified)
         `apply` optional (attribute (xsd "finalDefault") final e)
         `apply` optional (attribute (xsd "blockDefault") block e)
         `apply` optional (attribute (N "targetNamespace") uri  e)
         `apply` optional (attribute (N "version")       string e)
    --   `apply` interiorWith (not.xsdTag "annotation") (many schemaItem) e
         `apply` interiorWith (const True) (many schemaItem) e

-- | Parse a (possibly missing) <xsd:annotation> element.
annotation :: XsdParser Annotation
annotation = do
    definiteAnnotation `onFail` return (NoAnnotation "missing")
{-
annotation = do
    me <- optional (xsdElement "annotation")
    case me of
      Nothing -> return (NoAnnotation "missing")
      Just e -> (fmap Documentation $ interiorWith (xsdTag "documentation")
                                                   (allChildren text)  e)
                    `onFail`
                (fmap AppInfo $ interiorWith (xsdTag "appinfo")
                                             (allChildren text) e)
                    `onFail`
                return (NoAnnotation "failed to parse")
-}

-- | Parse a definitely-occurring <xsd:annotation> element.
definiteAnnotation :: XsdParser Annotation
definiteAnnotation = do
    e <- xsdElement "annotation"
    ( fmap Documentation $ interiorWith (xsdTag "documentation")
                                        (allChildren text)  e
      ) `onFail` (
      fmap AppInfo $ interiorWith (xsdTag "documentation")
                                        (allChildren text)  e
      ) `onFail` (
      return (NoAnnotation "failed to parse")
      )

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
block :: TextParser Block
block = final

-- | Parse a schema item (just under the toplevel <xsd:schema>)
schemaItem :: XsdParser SchemaItem
schemaItem = oneOf'
       [ ("xsd:include",        include)
       , ("xsd:import",         import_)
       , ("xsd:redefine",       redefine)
       , ("xsd:annotation",     fmap Annotation     definiteAnnotation)
         --
       , ("xsd:simpleType",     fmap Simple           simpleType)
       , ("xsd:complexType",    fmap Complex          complexType)
       , ("xsd:element",        fmap SchemaElement    elementDecl)
       , ("xsd:attribute",      fmap SchemaAttribute  attributeDecl)
       , ("xsd:attributeGroup", fmap AttributeGroup   attributeGroup)
       , ("xsd:group",          fmap SchemaGroup      group_)
   --  , ("xsd:notation",       notation)
       ]

-- | Parse an <xsd:include>.
include :: XsdParser SchemaItem
include = do e <- xsdElement "include"
             commit $ return Include
                      `apply` attribute (N "schemaLocation") uri e
                      `apply` interiorWith (xsdTag "annotation") annotation e

-- | Parse an <xsd:import>.
import_ :: XsdParser SchemaItem
import_ = do e <- xsdElement "import"
             commit $ return Import
                      `apply` attribute (N "namespace")      uri e
                      `apply` attribute (N "schemaLocation") uri e
                      `apply` interiorWith (xsdTag "annotation") annotation e

-- | Parse a <xsd:redefine>.
redefine :: XsdParser SchemaItem
redefine = do e <- xsdElement "redefine"
              commit $ return Redefine
                       `apply` attribute (N "schemaLocation") uri e
                       `apply` interiorWith (const True) (many schemaItem) e

-- | Parse a <xsd:simpleType> decl.
simpleType :: XsdParser SimpleType
simpleType = do e <- xsdElement "simpleType"
                n <- optional (attribute (N "name") (fmap N name) e)
                case n of
                  Nothing -> return (Primitive String) -- dummy for now
                  Just _  -> return (Primitive String) -- also a dummy

-- | Parse a <xsd:complexType> decl.
complexType :: XsdParser ComplexType
complexType =
    do e  <- xsdElement "complexType"
       commit $ return ComplexType
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` optional (attribute (N "name") string e)
           `apply` (attribute (N "abstract") bool e `onFail` return False)
           `apply` optional (attribute (N "final") final e)
           `apply` optional (attribute (N "block") block e)
           `apply` (attribute (N "mixed") bool e `onFail` return False)
           `apply` interiorWith (not . xsdTag "annotation") complexItem e

-- | Parse the alternative contents of a <xsd:complexType> decl.
complexItem :: XsdParser ComplexItem
complexItem =
    ( do e <- xsdElement "simpleContent"
         commit $ return SimpleContent
                `apply` interiorWith (xsdTag "annotation") annotation e
                `apply` interiorWith (not.xsdTag "annotation") stuff e
    ) `onFail` (
      do e <- xsdElement "complexContent"
         commit $ return ComplexContent
                `apply` interiorWith (xsdTag "annotation") annotation e
                `apply` (attribute (N "mixed") bool e `onFail` return False)
                `apply` interiorWith (not.xsdTag "annotation") stuff e
    ) `onFail` (
      do fmap ThisType $ particleAttrs
    )
  where
    stuff :: XsdParser (Either Restriction1 Extension)
    stuff =
      ( do e <- xsdElement "restriction"
           commit $ fmap Left $ return Restriction1 `apply` particle
      ) `onFail` (
        do e <- xsdElement "extension"
           commit $ fmap Right $ return Extension
                `apply` interiorWith (xsdTag "annotation") annotation e
                `apply` attribute (N "base") qname e
                `apply` interiorWith (not.xsdTag "annotation") particleAttrs e
      )

-- | Parse a particle decl.
particle :: XsdParser Particle
particle = optional (fmap Left choiceOrSeq `onFail` fmap Right group_)

-- | Parse a particle decl with optional attributes.
particleAttrs :: XsdParser ParticleAttrs
particleAttrs = return PA `apply` particle
                          `apply` many (fmap Left attributeDecl
                                        `onFail`
                                        fmap Right attributeGroup)
                          `apply` optional anyAttr

-- | Parse an <xsd:all>, <xsd:choice>, or <xsd:sequence> decl.
choiceOrSeq :: XsdParser ChoiceOrSeq
choiceOrSeq =
    do e <- xsdElement "all"
       commit $ return All
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` interiorWith (not.xsdTag "annotation") (many elementDecl) e
    `onFail`
    do e <- xsdElement "choice"
       commit $ return Choice
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` occurs e
           `apply` interiorWith (not.xsdTag "annotation") (many elementEtc) e
    `onFail`
    do e <- xsdElement "sequence"
       commit $ return Sequence
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` occurs e
           `apply` interiorWith (not.xsdTag "annotation") (many elementEtc) e

-- | Parse a <xsd:group> decl.
group_ :: XsdParser Group
group_ = do e <- xsdElement "group"
            commit $ return Group
                `apply` interiorWith (xsdTag "annotation") annotation e
                `apply` (fmap Left (attribute (N "name") string e)
                         `onFail`
                         fmap Right (attribute (N "ref") (fmap N string) e))
                `apply` occurs e
                `apply` interiorWith (not.xsdTag "annotation")
                                     (optional choiceOrSeq) e

-- | Parse an <xsd:element>, <xsd:group>, <xsd:all>, <xsd:choice>,
--   <xsd:sequence> or <xsd:any>.
elementEtc :: XsdParser ElementEtc
elementEtc = fmap HasElement elementDecl
             `onFail`
             fmap HasGroup group_
             `onFail`
             fmap HasCS choiceOrSeq
             `onFail`
             fmap HasAny any_

-- | Parse an <xsd:any>.
any_ :: XsdParser Any
any_ = do e <- xsdElement "any"
          return Any `apply` interiorWith (xsdTag "annotation") annotation e

-- | Parse an <xsd:anyAttribute>.
anyAttr :: XsdParser AnyAttr
anyAttr = do e <- xsdElement "anyAttribute"
             commit $ return AnyAttr
                 `apply` interiorWith (xsdTag "annotation") annotation e
                 `apply` (attribute (N "namespace") uri e
                          `onFail` return "##any")
                 `apply` (attribute (N "processContents") processContents e
                          `onFail` return Strict)

-- | Parse an <xsd:attributegroup>.
attributeGroup :: XsdParser AttrGroup
attributeGroup =
    do e <- xsdElement "attributeGroup"
       commit $ return AttrGroup
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` (fmap Left (attribute (N "name") string e)
                    `onFail`
                    fmap Right (attribute (N "ref") (fmap N string) e))
           `apply` interiorWith (not.xsdTag "annotation") (many stuff) e
  where
    stuff = fmap Left attributeDecl `onFail` fmap Right attributeGroup

-- | Parse an <xsd:element> decl.
elementDecl :: XsdParser ElementDecl
elementDecl =
    do e <- xsdElement "element"
       commit $ return ElementDecl
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` (fmap Left (nameAndType e)
                    `onFail`
                    fmap Right (attribute (N "ref") (fmap N string) e))
           `apply` occurs e
           `apply` (attribute (N "nillable") bool e `onFail` return False)
           `apply` optional (attribute (N "substitutionGroup") qname e)
           `apply` (attribute (N "abstract") bool e `onFail` return False)
           `apply` optional (attribute (xsd "final") final e)
           `apply` optional (attribute (xsd "block") block e)
           `apply` (attribute (xsd "form") qform e `onFail` return Unqualified)
           `apply` interiorWith (xsdTag "simpleType" ||| xsdTag "complexType")
                                (optional (fmap Left simpleType
                                           `onFail`
                                           fmap Right complexType)) e
           `apply` interiorWith (xsdTag "unique" ||| xsdTag "key"
                                                 ||| xsdTag "keyRef")
                                (many uniqueKeyOrKeyRef) e

-- | Parse name and type attributes.
nameAndType :: Element Posn -> XsdParser NameAndType
nameAndType e = return NT `apply` attribute (N "name") string e
                          `apply` optional (attribute (N "type") qname e)

-- | Parse an <xsd:attribute> decl.
attributeDecl :: XsdParser AttributeDecl
attributeDecl =
    do e <- xsdElement "attribute"
       commit $ return AttributeDecl
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` (fmap Left (nameAndType e)
                    `onFail`
                    fmap Right (attribute (N "ref") (fmap N string) e))
           `apply` (attribute (N "use") use e `onFail` return Optional)
           `apply` (optional (attribute (N "default") (fmap Left string) e
                              `onFail`
                              attribute (N "fixed") (fmap Right string) e))
           `apply` (attribute (xsd "form") qform e `onFail` return Unqualified)
           `apply` interiorWith (xsdTag "simpleType") (optional simpleType) e


-- | Parse an occurrence range from attributes of given element.
occurs :: Element Posn -> XsdParser Occurs
occurs e = return Occurs
               `apply` (optional $ attribute (N "minOccurs") parseDec e)
               `apply` (optional $ attribute (N "maxOccurs") parseDec e)

-- | Parse a <xsd:unique>, <xsd:key>, or <xsd:keyref>.
uniqueKeyOrKeyRef :: XsdParser UniqueKeyOrKeyRef
uniqueKeyOrKeyRef = fmap U unique `onFail` fmap K key `onFail` fmap KR keyRef

-- | Parse a <xsd:unique>.
unique :: XsdParser Unique
unique =
    do e <- xsdElement "unique"
       commit $ return Unique
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "name") string e
           `apply` interiorWith (xsdTag "selector") selector e
           `apply` interiorWith (xsdTag "field") (many1 field_) e

-- | Parse a <xsd:key>.
key :: XsdParser Key
key =
    do e <- xsdElement "key"
       commit $ return Key
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "name") string e
           `apply` interiorWith (xsdTag "selector") selector e
           `apply` interiorWith (xsdTag "field") (many1 field_) e

-- | Parse a <xsd:keyref>.
keyRef :: XsdParser KeyRef
keyRef =
    do e <- xsdElement "keyref"
       commit $ return KeyRef
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "name") string e
           `apply` attribute (N "refer") qname e
           `apply` interiorWith (xsdTag "selector") selector e
           `apply` interiorWith (xsdTag "field") (many1 field_) e

-- | Parse a <xsd:selector>.
selector :: XsdParser Selector
selector =
    do e <- xsdElement "selector"
       commit $ return Selector
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "xpath") string e

-- | Parse a <xsd:field>.
field_ :: XsdParser Field
field_ =
    do e <- xsdElement "field"
       commit $ return Field
           `apply` interiorWith (xsdTag "annotation") annotation e
           `apply` attribute (N "xpath") string e

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

-- | Parse a "processContents" attribute, i.e. "skip", "lax", or "strict".
processContents :: TextParser ProcessContents
processContents =
    do w <- word
       case w of
         "skip"   -> return Skip
         "lax"    -> return Lax
         "strict" -> return Strict
         _        -> fail "could not parse \"processContents\" attribute value"

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
