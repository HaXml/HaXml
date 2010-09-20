{-# LANGUAGE TypeSynonymInstances #-}
module Text.XML.HaXml.Schema.PrimitiveTypes
  ( -- * Type class for parsing simpleTypes
    SimpleType(..)
  , module Text.Parse
  , -- * Primitive XSD datatypes
    XsdString(..)
  , Boolean(..)
  , Base64Binary(..)
  , HexBinary(..)
  , Float(..)
  , Decimal(..)
  , Double(..)
  , AnyURI(..)
  , QName(..)
  , NOTATION(..)
  , Duration(..)
  , DateTime(..)
  , Time(..)
  , Date(..)
  , GYearMonth(..)
  , GYear(..)
  , GMonthDay(..)
  , GDay(..)
  , GMonth(..)
  , -- * Derived, yet builtin, datatypes
    NormalizedString(..)
  , Token(..)
  , Language(..)
  , Name(..)
  , NCName(..)
  , ID(..)
  , IDREF(..)
  , IDREFS(..)
  , ENTITY(..)
  , ENTITIES(..)
  , NMTOKEN(..)
  , NMTOKENS(..)
  , Integer(..)
  , NonPositiveInteger(..)
  , NegativeInteger(..)
  , Long(..)
  , Int(..)
  , Short(..)
  , Byte(..)
  , NonNegativeInteger(..)
  , UnsignedLong(..)
  , UnsignedInt(..)
  , UnsignedShort(..)
  , UnsignedByte(..)
  , PositiveInteger(..)
  ) where

import Text.Parse
import Data.Char as Char
--import Data.Time.LocalTime -- for dates and times?
import Text.XML.HaXml.Types (QName(..))
import Data.Int
import Data.Word

-- | Ultimately, an XML parser will find some plain text as the content
--   of a simpleType, which will need to be parsed.  We use a TextParser,
--   because values of simpleTypes can also be given elsewhere, e.g. as
--   attribute values in an XSD definition, e.g. to restrict the permissible
--   values of the simpleType.  Such restrictions are therefore implemented
--   as layered parsers.
class SimpleType a where
  acceptingParser :: TextParser a

-- * Primitive types

type Boolean      = Bool
newtype XsdString = XsdString    String    deriving (Eq,Show)
data Base64Binary = Base64Binary String    deriving (Eq,Show)
data HexBinary    = HexBinary    String    deriving (Eq,Show)
data AnyURI       = AnyURI       String    deriving (Eq,Show)
--data QName
data NOTATION     = NOTATION String -- or re-use NOTATION from HaXml.Types?
                                           deriving (Eq,Show)
data Decimal      = Decimal Double         deriving (Eq,Show)
--data Float
--data Double
data Duration     = Duration Bool Int Int Int Int Int Float  deriving (Eq,Show)
data DateTime     = DateTime               deriving (Eq,Show) -- LocalTime ?
data Time         = Time                   deriving (Eq,Show) -- TimeOfDay ?
data Date         = Date                   deriving (Eq,Show) -- Day ?
data GYearMonth   = GYearMonth             deriving (Eq,Show)
data GYear        = GYear                  deriving (Eq,Show)
data GMonthDay    = GMonthDay              deriving (Eq,Show)
data GDay         = GDay                   deriving (Eq,Show)
data GMonth       = GMonth                 deriving (Eq,Show)

isNext :: Char -> TextParser Char
isNext c = do d <- next
              if c==d then return c else fail ("expected "++c:", got "++d:".")

instance SimpleType Bool where
    acceptingParser = do w <- word
                         case w of "true"  -> return True;
                                   "false" -> return False
                                   "0"     -> return False;
                                   "1"     -> return True
                                   _       -> fail ("Not a bool: "++w)
instance SimpleType XsdString where
    acceptingParser = fmap XsdString word
instance SimpleType Base64Binary where
    acceptingParser = fmap Base64Binary (many (satisfy isAlphaNum `onFail`
                                               satisfy isSpace `onFail`
                                               satisfy (`elem`"+/=")))
instance SimpleType HexBinary where
    acceptingParser = fmap HexBinary (many (satisfy Char.isHexDigit))
instance SimpleType AnyURI where
    acceptingParser = fmap AnyURI (many next) -- not very satisfactory
instance SimpleType NOTATION where
    acceptingParser = fmap NOTATION (many next) -- not very satisfactory

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
                                      `apply` ((parseDec `discard` isNext 'Y')
                                               `onFail` return 0)
                                      `apply` ((parseDec `discard` isNext 'M')
                                               `onFail` return 0)
                                      `apply` ((parseDec `discard` isNext 'D')
                                               `onFail` return 0)
                                      `discard` (isNext 'T'`onFail`return 'T')
                                      -- fix: T absent iff H:M:S absent also
                                      `apply` ((parseDec `discard` isNext 'H')
                                               `onFail` return 0)
                                      `apply` ((parseDec `discard` isNext 'M')
                                               `onFail` return 0)
                                      `apply` ((parseFloat `discard` isNext 'S')
                                               `onFail` return 0)
instance SimpleType DateTime where
    acceptingParser = fail "not implemented: simpletype parser for DateTime"
instance SimpleType Time where
    acceptingParser = fail "not implemented: simpletype parser for Time"
instance SimpleType Date where
    acceptingParser = fail "not implemented: simpletype parser for Date"
instance SimpleType GYearMonth where
    acceptingParser = fail "not implemented: simpletype parser for GYearMonth"
instance SimpleType GYear where
    acceptingParser = fail "not implemented: simpletype parser for GYear"
instance SimpleType GMonthDay where
    acceptingParser = fail "not implemented: simpletype parser for GMonthDay"
instance SimpleType GDay where
    acceptingParser = fail "not implemented: simpletype parser for GDay"
instance SimpleType GMonth where
    acceptingParser = fail "not implemented: simpletype parser for GMonth"

-- * Derived builtin types

newtype NormalizedString = Normalized String	deriving (Eq,Show)
newtype Token    = Token    String              deriving (Eq,Show)
newtype Language = Language String              deriving (Eq,Show)
newtype Name     = Name     String              deriving (Eq,Show)
newtype NCName   = NCName   String              deriving (Eq,Show)
newtype ID       = ID       String              deriving (Eq,Show)
newtype IDREF    = IDREF    String              deriving (Eq,Show)
newtype IDREFS   = IDREFS   String              deriving (Eq,Show)
newtype ENTITY   = ENTITY   String              deriving (Eq,Show)
newtype ENTITIES = ENTITIES String              deriving (Eq,Show)
newtype NMTOKEN  = NMTOKEN  String              deriving (Eq,Show)
newtype NMTOKENS = NMTOKENS String              deriving (Eq,Show)

instance SimpleType NormalizedString where
    acceptingParser = fmap Normalized (many next)
instance SimpleType Token where
    acceptingParser = fmap Token (many next)
instance SimpleType Language where
    acceptingParser = fmap Language (many next)
instance SimpleType Name where
    acceptingParser = fmap Name (many next)
instance SimpleType NCName where
    acceptingParser = fmap NCName (many next)
instance SimpleType ID where
    acceptingParser = fmap ID (many next)
instance SimpleType IDREF where
    acceptingParser = fmap IDREF (many next)
instance SimpleType IDREFS where
    acceptingParser = fmap IDREFS (many next)
instance SimpleType ENTITY where
    acceptingParser = fmap ENTITY (many next)
instance SimpleType ENTITIES where
    acceptingParser = fmap ENTITIES (many next)
instance SimpleType NMTOKEN where
    acceptingParser = fmap NMTOKEN (many next)
instance SimpleType NMTOKENS where
    acceptingParser = fmap NMTOKENS (many next)

--data Integer
newtype NonPositiveInteger = NonPos   Integer   deriving (Eq,Show)
newtype NegativeInteger    = Negative Integer   deriving (Eq,Show)
newtype Long               = Long     Int64     deriving (Eq,Show)
--data Int
newtype Short              = Short    Int16     deriving (Eq,Show)
newtype Byte               = Byte     Int8      deriving (Eq,Show)
newtype NonNegativeInteger = NonNeg   Integer   deriving (Eq,Show)
newtype UnsignedLong       = ULong    Word64    deriving (Eq,Show)
newtype UnsignedInt        = UInt     Word32    deriving (Eq,Show)
newtype UnsignedShort      = UShort   Word16    deriving (Eq,Show)
newtype UnsignedByte       = UByte    Word8     deriving (Eq,Show)
newtype PositiveInteger    = Positive Integer   deriving (Eq,Show)

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

