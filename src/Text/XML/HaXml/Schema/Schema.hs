{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Text.XML.HaXml.Schema.Schema
  ( SchemaType(..)
  , SchemaAttribute(..)
  , Extension(..)
  , Restricts(..)
  , getAttribute
  , between
  , parseSimpleType
  , module Text.XML.HaXml.XmlContent.Parser
  , module Text.ParserCombinators.Poly
  , module Text.XML.HaXml.Schema.PrimitiveTypes
  ) where

import Text.ParserCombinators.Poly
import Text.Parse

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces (printableName)
import Text.XML.HaXml.XmlContent.Parser
import Text.XML.HaXml.Schema.XSDTypeModel (Occurs(..))
import Text.XML.HaXml.Schema.PrimitiveTypes

-- | A SchemaType is for element types, and has a parser from generic XML
--   content tree to a Haskell value.
class SchemaType a where
    parseSchemaType      :: String -> XMLParser a
    contentsOfSchemaType :: XMLParser a -- XXX ditch it

-- | A SchemaAttribute has a parser from String (the text of the attribute)
--   to a Haskell value.
class Show a => SchemaAttribute a where
    attributeValue :: Parser String a

-- | A type t can extend another type s by the addition of extra fields e.
--   s is therefore the supertype of t, and e is s's extension of t.
class Extension t s e | t -> s e where
    supertype :: t -> s
    extension :: t -> e
    contentsOfExtension :: XMLParser e -- XXX ditch it

-- possibly, extension should be more simple, allowing only downcasting
class Extension t s | t -> s where
    supertype :: t -> s

-- | A type t can restrict another type s, that is, t admits fewer values
--   than s, but all the values t does admit also belong to the type s.
class Restricts t s   | t -> s where
    restricts :: t -> s

-- | Given a TextParser for a SimpleType, make it into an XMLParser, i.e.
--   consuming textual XML content as input rather than a String.
parseSimpleType :: SimpleType t => XMLParser t
parseSimpleType = do s <- text
                     case runParser acceptingParser s of
                       (Left err, _) -> fail err
                       (Right v, "") -> return v
                       (Right v, _)  -> return v -- ignore trailing text

-- | Between is a list parser that tries to ensure that any range
--   specification (min and max elements) is obeyed when parsing.
between :: PolyParse p => Occurs -> p a -> p [a]
between (Occurs Nothing  Nothing)  p = fmap (:[]) p
between (Occurs (Just i) Nothing)  p = fmap (++) `apply` exactly i p
                                                 `apply` many p
between (Occurs Nothing  (Just j)) p = upto j p
between (Occurs (Just i) (Just j)) p = fmap (++) `apply` exactly i p
                                                 `apply` upto (j-i) p

-- | Generated parsers will use 'getAttribute' as a convenient wrapper
--   to lift a SchemaAttribute parser into an XMLParser.
getAttribute :: (SchemaAttribute a) =>
                String -> Element Posn -> Posn -> XMLParser a
getAttribute aname (Elem t as _) pos =
    case qnLookup aname as of
        Nothing  -> fail $ "attribute missing: " ++ aname
                           ++ " in element <" ++ t ++ "> at " ++ show pos
        Just atv -> case runParser attributeValue (attr2str atv) of
                        (Right val, "")   -> return val
                        (Right val, rest) -> failBad $
                                               "Bad attribute value for "
                                               ++ aname ++ " in element <"
                                               ++ t ++ ">:  got "++show val
                                               ++ "\n but trailing text: "
                                               ++ rest ++ "\n at " ++ show pos
                        (Left err,  rest) -> failBad $ err ++ " in attribute "
                                               ++ aname  ++ " of element <"
                                               ++ t ++ "> at " ++ show pos
  where
    qnLookup :: String -> [(QName,a)] -> Maybe a
    qnLookup s = Prelude.lookup s . map (\(qn,v)-> (printableName qn, v)



{- examples
   --------

instance SchemaType FpMLSomething where
  parseSchemaType s = do (pos,e) <- posnElement [s]
                         commit $ do
                           a0 <- getAttribute "flirble" e pos
                           a1 <- getAttribute "binky" e pos
                           interior e $ do
                             c0 <- parseSchemaType "foobar"
                             c1 <- many $ parseSchemaType "quux"
                             c2 <- optional $ parseSchemaType "doodad"
                             c3 <- between (Occurs (Just 3) (Just 5))
                                            $ parseSchemaType "rinta"
                             return $ FpMLSomething a0 a1 c0 c1 c2 c3

instance SchemaAttribute FpMLNumber where
    attributeValue = ...
-}
