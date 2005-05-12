
DrIFT version 1.1
-- Based on DrIFT 1.0 by Noel Winstanley
--  hacked for Haskell 98 by Malcolm Wallace, University of York, Feb 1999.

#if defined(__HASKELL98__)
#define FMAP fmap
#else
#define FMAP map
#endif

>module Main where
>import ChaseImports
>import UserRules (userRules)
>import StandardRules (Tag,Rule,standardRules)
>import RuleUtils (commentLine,texts)
>import PreludData(preludeData)
>import DataP
>import Pretty 
>import List (partition)
>import System

>main = do	
>       l <- getArgs
>	putStrLn "{-% DrIFT (Automatic class derivations for Haskell) v1.1 %-}"
>	derive l

>derive l = case l of		
>    [f] -> derive' f
>    _	 -> error "Incorrect number of Args."
>  where
>    derive' fname =
>      do
>	file <- readFile fname
>       let (body,_) = userCode file
>           b = isLiterate body
>	    (docs,dats,todo) = process  . parser . fromLit b $ body
>	moreDocs <- FMAP ((\(x,_,_) -> x) . process) (chaseImports body todo)
>	let result = toLit b . (\r -> codeSeperator ++ '\n':r) . 
>			render . vsep $ (docs ++ sepDoc:moreDocs)
>  --	writeFile (backup fname) file
>  --   writeFile (newfile fname) (body ++ result)
>       putStrLn (body ++ result)

>rules = userRules ++ standardRules
>-- codeRender doc = fullRender PageMode 80 1 doc "" -- now obsolete
>vsep = vcat . map ($$ (text ""))
>sepDoc = commentLine . text $ " Imported from other files :-"

>backup :: FilePath -> FilePath
>backup f = (reverse . dropWhile (/= '.') . reverse ) f ++ "bak"

>newfile :: FilePath -> FilePath
>newfile f = (reverse . dropWhile (/= '.') . reverse ) f ++ "DrIFT"

Main Pass - Takes parsed data and rules and combines to create instances...
Returns all parsed data, ande commands calling for files to be imported if
datatypes aren't located in this module.

>process :: ToDo -> ([Doc],[Data],ToDo)
>process i = (concatMap g dats ++ concatMap h moreDats,parsedData,imports)
>       where
>	g (tags,d) = [(find t rules) d | t <- (tags ++ directives)]
>	h (tags,d) = [(find t rules) d | t <- tags]
>	directives = concatMap fst globals
>	(dats,commands) = partition (isData . snd) i
>	(globals,fors) = partition (\(_,d) -> d == Directive) commands
>	(moreDats,imports) = resolve parsedData fors ([],[])
>	parsedData = map snd dats ++ preludeData

>find :: Tag -> [Rule] -> (Data -> Doc)
>find t r = case filter ((==t) . fst) $ r of
>               [] -> const (commentLine warning)
>               (x:xs) -> snd x
>   where
>   warning = hsep . texts $ ["Warning : Rule",t,"not found."]                 

