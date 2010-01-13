import Felgenhauer
import Text.XML.HaXml.XmlContent

main = let a = readXml "<broken>abc</broken>" :: Either String Broken
           b = readXml "<broken></broken>"    :: Either String Broken
           c = readXml "<simple>abc</simple>" :: Either String Simple
           d = readXml "<simple></simple>"    :: Either String Simple
       in do
             print c
             print d
             print a
             print b
