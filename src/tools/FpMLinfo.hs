-- FpMLinfo
module Main where

-- This program is designed to gather information from a bunch of XML files
-- containing XSD module decls.

import System.Exit
import System.Environment
import System.IO
import Control.Monad
import System.Directory
import Data.List
import Data.Maybe (fromMaybe,catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (on)
import Data.Monoid (mconcat)
--import Either

import Text.XML.HaXml            (version)
import Text.XML.HaXml.Types hiding (Choice)
import Text.XML.HaXml.Namespaces (resolveAllNames,qualify,printableName
                                 ,nullNamespace)
import Text.XML.HaXml.Parse      (xmlParse')
import Text.XML.HaXml.Util       (docContent)
import Text.XML.HaXml.Posn       (posInNewCxt)

import Text.XML.HaXml.Schema.Parse
import Text.XML.HaXml.Schema.NameConversion
import Text.XML.HaXml.Schema.Environment    as Env
import Text.XML.HaXml.Schema.TypeConversion as XsdToH
import Text.XML.HaXml.Schema.PrettyHaskell
import Text.XML.HaXml.Schema.XSDTypeModel
import qualified Text.XML.HaXml.Schema.HaskellTypeModel as Haskell
import Text.ParserCombinators.Poly
import Text.PrettyPrint.HughesPJ (render,vcat,nest,text,($$))

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- sucked in from Text.XML.HaXml.Wrappers to avoid dependency on T.X.H.Html
argDirsToFiles :: IO (FilePath,[FilePath])
argDirsToFiles = do
  args <- getArgs
  when ("--version" `elem` args) $ do
      putStrLn $ "part of HaXml-"++version
      exitWith ExitSuccess
  when ("--help" `elem` args) $ do
      putStrLn $ "Usage: FpMLinfo xsdDir"
      exitWith ExitSuccess
  case args of
    [xsddir]-> do
            files <- fmap (filter (".xsd" `isSuffixOf`))
                          (getDirectoryContents xsddir)
            return (xsddir, files)
    _ -> do prog <- getProgName
            putStrLn ("Usage: "++prog++" xsdDir")
            exitFailure
 where
  reslash = map (\c-> case c of '.'->'/'; _->c)
  dirOf   = concat . intersperse "/" . init . wordsBy '.'
  wordsBy c s = let (a,b) = span (/=c) s in
                if null b then [a] else a: wordsBy c (tail b)

main ::IO ()
main = do
    (dir,files) <- argDirsToFiles
    deps <- flip mapM files (\ inf-> do
        hPutStrLn stderr $ "Reading "++inf
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

    let filedeps :: [[(FilePath,([(FilePath,Maybe String)],Schema))]]
        filedeps  = ordered (\ (inf,_)-> inf)
                            (\ (_,(ds,_))-> map fst ds)
                            (\x-> lookupWith fst x (zip files deps))
                            (zip files deps)
        -- a single supertype environment, closed over all modules
        supertypeEnv :: Environment
        supertypeEnv = foldr (\fs e->
                              foldr (\(inf,(_,v))-> mkEnvironment inf v) e fs)
                             emptyEnv filedeps

        adjust :: Environment -> Environment
        adjust env = env{ env_extendty = env_extendty supertypeEnv
                        , env_substGrp = env_substGrp supertypeEnv }

        -- each module's env includes only dependencies, apart from supertypes
        environs :: [(FilePath,(Environment,Schema))]
        environs  = flip concatMap filedeps (\scc->
                      case scc of
                        [(inf,(ds,v))]->
                          [(inf, ( adjust $ mkEnvironment inf v
                                       (foldr combineEnv emptyEnv
                                           (flip map ds
                                               (\d-> fst $
                                                     fromMaybe (error "FME") $
                                                     lookup (fst d) environs)
                                           )
                                       )
                                 , v
                                 )
                          )]
                        cyclic ->
                            let jointSchema :: Schema
                                jointSchema = mconcat (map (snd.snd) cyclic)
                                jointDeps :: [FilePath]
                                jointDeps = concatMap (map fst.fst.snd) cyclic
                                jointEnv :: Environment
                                jointEnv = mkEnvironment "" jointSchema $
                                           foldr combineEnv emptyEnv $
                                           flip map (nub jointDeps
                                                    \\ map fst cyclic)
                                               (\d-> fst $
                                                     fromMaybe (error "FME") $
                                                     lookup d environs)
                            in flip map cyclic
                                    (\(inf,(_,v))->
                                      (inf,(adjust $ mkEnvironment inf v
                                                   $ jointEnv
                                           ,v)
                                      )
                                    )
                    )

    putStrLn $ "Supertype environment:\n----------------------"
    putStrLn . display . env_extendty $ supertypeEnv
    putStrLn ""
    putStrLn $ "Substitution group environment:\n------------------------------"
    putStrLn . display . env_substGrp $ supertypeEnv
    putStrLn ""
    putStrLn $ "Type containment relation:\n--------------------------"
    putStrLn . unlines . Prelude.map (\k-> printableName k++": "++
                                           (unwords . nub . map printableName
                                             . contains supertypeEnv $ k))
             . Map.keys . env_type $ supertypeEnv
    putStrLn ""
    putStrLn $ "Type cycles:\n------------"
    putStrLn . unlines . map unwords . cycles $ supertypeEnv
    putStrLn ""
    putStrLn $ "Module dependency ordering:\n---------------------------"
    putStrLn . unlines
             . concatMap (map (\(inf,(deps,_)) ->
                                 inf++": "++unwords (map fst deps)))
             $ filedeps

    putStrLn ""
    putStrLn $ "Module cycles:\n--------------"
    putStrLn . unlines . map (unwords . map fst)
             . cyclicDeps (\(inf,_)->inf)
                          (\(_,(ds,_))->map fst ds)
                          (\x-> lookupWith fst x (zip files deps))
             $ zip files deps


-- | Pretty print the names involved in a super/subtype (or substitution group)
--   environment.
display :: Map QName [(QName,FilePath)] -> String
display = render . vcat . Map.elems . Map.mapWithKey (\k v->
              text (printableName k) $$
              vcat (Prelude.map (nest 4 . text . printableName . fst) v))

-- | To a first approximation, what element types could appear directly inside
--   the given element type?  (Attribute types are not of interest here.)
contains :: Environment -> QName -> [QName]
contains env qn =
    case Map.lookup qn (env_type env) of
        Nothing -> []
        Just (Left s) -> simple s
        Just (Right c@ComplexType{complex_content=SimpleContent{}})  ->
            case ci_stuff (complex_content c) of
                Right e@Extension{}   -> [extension_base e]
                Left (Restriction1 p) -> particle p
        Just (Right c@ComplexType{complex_content=ComplexContent{}}) ->
            case ci_stuff (complex_content c) of
                Right e@Extension{}   -> [extension_base e]
                                         ++ particleAttrs (extension_newstuff e)
                Left (Restriction1 p) -> particle p
        Just (Right c@ComplexType{complex_content=ThisType{}})       ->
            particleAttrs . ci_thistype . complex_content $ c

  where
    simple s@Primitive{}  = (:[]) . N . show . simple_primitive $ s
    simple s@Restricted{} = maybe [] (:[]) . restrict_base . simple_restriction
                                                                   $ s
    simple s@ListOf{}     = either simple (const []) . simple_type $ s
    simple s@UnionOf{}    = concatMap simple (simple_union s)
    particleAttrs (PA p _ _) = particle p
    particle = maybe [] (either choiceOrSeq group)
    choiceOrSeq (All        _ es) = concatMap elementDecl es
    choiceOrSeq (Choice   _ _ es) = concatMap elementEtc es
    choiceOrSeq (Sequence _ _ es) = concatMap elementEtc es
    group = maybe [] choiceOrSeq . group_stuff
    elementEtc (HasElement e) = elementDecl e
    elementEtc (HasGroup   g) = group g
    elementEtc (HasCS     cs) = choiceOrSeq cs
    elementEtc (HasAny     _) = []
    elementDecl = either (maybe [] (:[]) . theType)
                         (:[])
                 . elem_nameOrRef

-- | Find cycles in recursive type schemes.
cycles :: Environment -> [[String]]
cycles env =
    concatMap (map (map printableName) . walk []) . Map.keys . env_type $ env
  where
    walk :: [QName] -> QName -> [[QName]]
    walk acc t = if not (null acc) && t == head acc then [acc]
                 else if t `elem` acc then [N "*": acc]
                 else let uses = contains env t in
                      if null uses then []
                      else concatMap (walk (acc++[t])) uses

-- | Munge filename for instances.
insts :: FilePath -> FilePath
insts x = case reverse x of
            's':'h':'.':f -> reverse f++"Instances.hs"
            _ -> error "bad stuff made my brains melt"


-- | Calculate dependency ordering of modules, least dependent first.
--   Cyclic groups may occur, suitably placed in the ordering.
ordered :: (Eq a, Eq b) => (b->a) -> (b->[a]) -> (a->Maybe b) -> [b] -> [[b]]
ordered name deps env list =
    let cycles    = cyclicDeps name deps env list
        noncyclic = map (:[]) $ list \\ concat cycles
        workqueue = noncyclic++cycles
    in traverse [] workqueue
  where
    traverse acc []     = acc
    traverse acc (w:wq) = if all (`elem` concatMap (map name) acc)
                                 (concatMap deps w \\ map name w)
                          then traverse (acc++[w]) wq
                          else traverse     acc   (wq++[w])

-- | Find cyclic dependencies between modules.
cyclicDeps :: Eq a => (b->a) -> (b->[a]) -> (a->Maybe b) -> [b] -> [[b]]
cyclicDeps name deps env = nubBy (setEq`on`map name)
                           . (\cs-> foldl minimal cs cs)
                           . concatMap (walk [])
  where
--  walk :: [b] -> b -> [[b]]
    walk acc t = if name t `elem` map name acc then [acc]
                 else concatMap (walk (t:acc)) (catMaybes . map env $ deps t)
    minimal acc c = concatMap (prune c) acc
    prune c c' = if map name c `isProperSubsetOf` map name c' then [] else [c']
    isSubsetOf a b = all (`elem`b) a
    setEq a b            = a`isSubsetOf`b &&      b`isSubsetOf`a
    isProperSubsetOf a b = a`isSubsetOf`b && not (b`isSubsetOf`a)

-- | A variation on the standard lookup function.
lookupWith :: Eq a => (b->a) -> a -> [b] -> Maybe b
lookupWith proj x [] = Nothing
lookupWith proj x (y:ys) | proj y == x = Just y
                         | otherwise   = lookupWith proj x ys


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

