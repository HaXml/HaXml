-- | This is just a convenient way of bunching the XML combinators
--   together with some other things you are likely to want at the
--   same time.
module Text.Xml.HaXml
  ( module Text.Xml.HaXml.Types
  , module Text.Xml.HaXml.Combinators
  , module Text.Xml.HaXml.Parse
  , module Text.Xml.HaXml.Pretty
  , module Text.Xml.HaXml.Html.Generate
  , module Text.Xml.HaXml.Html.Parse
  , module Text.Xml.HaXml.Validate
  , module Text.Xml.HaXml.Wrappers
  , render
  ) where

import Text.Xml.HaXml.Types
import Text.Xml.HaXml.Combinators
import Text.Xml.HaXml.Parse       (xmlParse,dtdParse)
import Text.Xml.HaXml.Pretty      (element)
import Text.Xml.HaXml.Html.Generate
import Text.Xml.HaXml.Html.Parse  (htmlParse)
import Text.Xml.HaXml.Validate    (validate)
import Text.Xml.HaXml.Wrappers    (fix2Args,processXmlWith)

import Text.PrettyPrint.HughesPJ  (render)

