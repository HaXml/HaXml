module Text.XML.HaXml.Schema.PrimitiveTypes
  ( -- * Type class for parsing simpleTypes
    SimpleType(..)
  , -- * Primitive XSD datatypes
    String
  , Bool
  , Base64Binary
  , HexBinary
  , Float
  , Decimal
  , Double
  , AnyURI
  , QName
  , NOTATION
  , Duration
  , DateTime
  , Time
  , Date
  , GYearMonth
  , GYear
  , GMonthDay
  , GDay
  , GMonth
  , -- * Derived, yet builtin, datatypes
    NormalizedString
  , Token
  , Language
  , Name
  , NCName
  , ID
  , IDREF
  , IDREFS
  , ENTITY
  , ENTITIES
  , NMTOKEN
  , NMTOKENS
  , Integer
  , NonPositiveInteger
  , NegativeInteger
  , Long
  , Int
  , Short
  , Byte
  , NonNegativeInteger
  , UnsignedLong
  , UnsignedInt
  , UnsignedShort
  , UnsignedByte
  , PositiveInteger
  ) where

import Text.Parse
import Data.Char as Char
--import Data.Time.LocalTime -- for dates and times?
import Text.XML.HaXml.Types (QName(..))

-- | Ultimately, an XML parser will find some plain text as the content
--   of a simpleType, which will need to be parsed.  We use a TextParser,
--   because values of simpleTypes can also be given elsewhere, e.g. as
--   attribute values in an XSD definition, e.g. to restrict the permissible
--   values of the simpleType.  Such restrictions are therefore implemented
--   as layered parsers.
class SimpleType a where
  acceptingParser :: TextParser a

-- * Primitive types

--data Bool
--data String
data Base64Binary = Base64Binary String
data HexBinary    = HexBinary    String
data AnyURI       = AnyURI       String
--data QName
data NOTATION     = NOTATION String -- or re-use NOTATION from HaXml.Types?
data Decimal      = Decimal Double
--data Float
--data Double
data Duration     = Duration Bool Int Int Int Int Int Float
data DateTime     = DateTime -- LocalTime ?
data Time         = Time     -- TimeOfDay ?
data Date         = Date     -- Day ?
data GYearMonth   = GYearMonth
data GYear        = GYear
data GMonthDay    = GMonthDay
data GDay         = GDay
data GMonth       = GMonth

isNext :: Char -> TextParser Char
isNext c = do d <- next
              if c==d then return c else fail ("expected "++c:", got "++d:".")

instance SimpleType Bool where
    acceptingParser = do w <- word
                         case w of "true"  -> True;  "false" -> False
                                   "0"     -> False; "1"     -> True
                                   _       -> fail ("Not a bool: "++w)
instance SimpleType String where
    acceptingParser = word
instance SimpleType Base64Binary where
    acceptingParser = fmap Base64Binary (many (satisfy isAlphaNum `onFail`
                                               satisfy isSpace `onFail`
                                               satisfy (`elem`"+/=")))
instance SimpleType HexBinary where
    acceptingParser = fmap HexBinary (many (satisfy Char.isHexDigit))
instance SimpleType AnyURI where
    acceptingParser = fmap AnyURI (many next) -- not very satisfactory

instance SimpleType Decimal where
    acceptingParser = fmap Decimal parse
instance SimpleType Float where
    acceptingParser = parse
instance SimpleType Double where
    acceptingParser = parse

instance SimpleType Duration where
    acceptingParser = return Duration `apply` (do isNext '-'; return False
                                               `onFail` return True)
                                      `discard` isNext 'P'
                                      `apply` (parseDec `discard` isNext 'Y'
                                               `onFail` return 0)
                                      `apply` (parseDec `discard` isNext 'M'
                                               `onFail` return 0)
                                      `apply` (parseDec `discard` isNext 'D'
                                               `onFail` return 0)
                                      `discard` (isNext 'T'`onFail`return 'T')
                                      -- fix: T absent iff H:M:S absent also
                                      `apply` (parseDec `discard` isNext 'H'
                                               `onFail` return 0)
                                      `apply` (parseDec `discard` isNext 'M'
                                               `onFail` return 0)
                                      `apply` (parseFloat `discard` isNext 'S'
                                               `onFail` return 0)

-- * Derived builtin types

newtype NormalizedString = Normalized String
newtype Token    = Token    String
newtype Language = Language String
newtype Name     = Name     String
newtype NCName   = NCName   String
newtype ID       = ID       String
newtype IDREF    = IDREF    String
newtype IDREFS   = IDREFS   String
newtype ENTITY   = ENTITY   String
newtype ENTITIES = ENTITIES String
newtype NMTOKEN  = NMTOKEN  String
newtype NMTOKENS = NMTOKENS String
--data Integer
newtype NonPositiveInteger = NonPos   Integer
newtype NegativeInteger    = Negative Integer
newtype Long               = Long     Int64
--data Int
newtype Short              = Short    Int16
newtype Byte               = Byte     Int8
newtype NonNegativeInteger = NonNeg   Integer
newtype UnsignedLong       = ULong    Word64
newtype UnsignedInt        = UInt     Word32
newtype UnsignedShort      = UShort   Word16
newtype UnsignedByte       = UByte    Word8
newtype PositiveInteger    = Positive Integer

instance SimpleType Integer where
    acceptingParser = parse
instance SimpleType NonPositiveInteger where
    acceptingParser = fmap NonPos parse
instance SimpleType NegativeInteger where
    acceptingParser = fmap Negative parse
instance SimpleType Long where
    acceptingParser = fmap (Long . fromInteger) parse
instance SimpleType Int where
    acceptingParser = parse
instance SimpleType Short where
    acceptingParser = fmap (Short . fromInteger) parse
instance SimpleType Byte where
    acceptingParser = fmap (Byte . fromInteger) parse
instance SimpleType NonNegativeInteger where
    acceptingParser = fmap NonNeg parse
instance SimpleType UnsignedLong where
    acceptingParser = fmap (ULong . fromInteger) parse
instance SimpleType UnsignedInt where
    acceptingParser = fmap (UInt . fromInteger) parse
instance SimpleType UnsignedShort where
    acceptingParser = fmap (UShort . fromInteger) parse
instance SimpleType UnsignedByte where
    acceptingParser = fmap (UByte . fromInteger) parse
instance SimpleType PositiveInteger where
    acceptingParser = fmap Positive parse

