-- FpMLToHaskell
module Main where

-- This program is designed to convert a bunch of XML files containing XSD
-- module decls into a bunch of Haskell modules containing data/newtype
-- definitions which mirror the XSD.  Once you have used this program
-- to generate your type definitions, you should import Text.XML.HaXml.Schema
-- (as well as the generated modules) wherever you intend to read and write
-- XML files with your Haskell programs.

import System
import IO
import Monad
import System.Directory
import List
import Maybe (fromMaybe)
--import Either

import Text.XML.HaXml            (version)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces (resolveAllNames,qualify
                                 ,nullNamespace)
import Text.XML.HaXml.Parse      (xmlParse')
import Text.XML.HaXml.Util       (docContent)
import Text.XML.HaXml.Posn       (posInNewCxt)

import Text.XML.HaXml.Schema.Parse
import Text.XML.HaXml.Schema.NameConversion
import Text.XML.HaXml.Schema.Environment    as Env
import Text.XML.HaXml.Schema.TypeConversion as XsdToH
import Text.XML.HaXml.Schema.PrettyHaskell
import Text.XML.HaXml.Schema.XSDTypeModel (Schema)
import qualified Text.XML.HaXml.Schema.HaskellTypeModel as Haskell
import Text.ParserCombinators.Poly
import Text.PrettyPrint.HughesPJ (render,vcat)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- sucked in from Text.XML.HaXml.Wrappers to avoid dependency on T.X.H.Html
argDirsToFiles :: IO (FilePath,[(FilePath,FilePath)])
argDirsToFiles = do
  args <- getArgs
  when ("--version" `elem` args) $ do
      putStrLn $ "part of HaXml-"++version
      exitWith ExitSuccess
  when ("--help" `elem` args) $ do
      putStrLn $ "Usage: FpMLToHaskell xsdDir haskellDir"
      putStrLn $ "    -- The results go into haskelldir/Data/FpML/file0.hs etc"
      putStrLn $ "See http://haskell.org/HaXml"
      exitWith ExitSuccess
  case args of
    [xsddir,hdir]-> do
            files <- fmap (filter (".xsd" `isSuffixOf`))
                          (getDirectoryContents xsddir)
            let newdirs = map (\file->hdir++"/"++dirOf (fpml file)) files
            mapM_ (\newdir -> do createDirectoryIfMissing True newdir) newdirs
            return (xsddir
                   ,map (\f-> (f, hdir++"/"++(reslash (fpml f))++".hs")) files)
    _ -> do prog <- getProgName
            putStrLn ("Usage: "++prog++" xsdDir haskellDir")
            exitFailure
 where
  reslash = map (\c-> case c of '.'->'/'; _->c)
  dirOf   = concat . intersperse "/" . init . wordsBy '.'
  wordsBy c s = let (a,b) = span (/=c) s in
                if null b then [a] else a: wordsBy c (tail b)

main ::IO ()
main = do
    (dir,files) <- argDirsToFiles
    deps <- flip mapM files (\ (inf,outf)-> do
        hPutStrLn stdout $ "Reading "++inf
        thiscontent <- readFile (dir++"/"++inf)
        let d@Document{} = resolveAllNames qualify
                           . either (error . ("not XML:\n"++)) id
                           . xmlParse' inf
                           $ thiscontent
        case runParser schema [docContent (posInNewCxt inf Nothing) d] of
            (Left msg,_) -> do hPutStrLn stderr msg
                               return ([], undefined)
            (Right v,[]) ->    return (Env.gatherImports v, v)
            (Right v,_)  -> do hPutStrLn stdout $ "Parse incomplete!"
                               hPutStrLn stdout $ inf
                               hPutStrLn stdout $ "\n-----------------\n"
                               hPutStrLn stdout $ show v
                               hPutStrLn stdout $ "\n-----------------\n"
                               return ([],v)
        )
    let filedeps :: [((FilePath,FilePath),([(FilePath,Maybe String)],Schema))]
        filedeps  = ordered (\ ((inf,_),_)-> inf)
                            (\ (_,(ds,_))-> map fst ds)
                            (zip files deps)
        environs :: [(FilePath,(Environment,FilePath,Schema))]
        environs  = flip map filedeps (\((inf,outf),(ds,v))->
                        ( inf, ( mkEnvironment v
                                     (foldr combineEnv emptyEnv
                                            (flip map ds
                                                  (\d-> fst3 $
                                                        fromMaybe (error "FME")$
                                                        lookup (fst d) environs)
                                            )
                                     )
                               , outf
                               , v
                               )
                        )
                    )
    flip mapM_ environs (\ (inf,(env,outf,v))-> do
        o <- openFile outf WriteMode
        let decls   = XsdToH.convert env v
            haskell = Haskell.mkModule inf v decls
            doc     = ppModule fpmlNameConverter haskell
        hPutStrLn stdout $ "Writing "++outf
        hPutStrLn o $ render doc
        hFlush o
        )


-- | Calculate dependency ordering of modules, least dependent first.
ordered :: Eq a => (b->a) -> (b->[a]) -> [b] -> [b]
ordered name deps = foldr insert []
  where
    insert x q = peelOff (deps x) x q
    peelOff [] x q     = x:q
    peelOff ds x []    = x:[]
    peelOff ds x (a:q) | any (== name a) ds = a: peelOff (ds\\[name a]) x q
                       | otherwise          = a: peelOff ds             x q

-- | What is the targetNamespace of the unique top-level element?
targetNamespace :: Element i -> String
targetNamespace (Elem qn attrs _) =
    if qn /= xsdSchema then "ERROR! top element not an xsd:schema tag"
    else case lookup (N "targetNamespace") attrs of
           Nothing -> "ERROR! no targetNamespace specified"
           Just atv -> show atv

-- | The XSD Namespace.
xsdSchema :: QName
xsdSchema = QN (nullNamespace{nsURI="http://www.w3.org/2001/XMLSchema"})
               "schema"

