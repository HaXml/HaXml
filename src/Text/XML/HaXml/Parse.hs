-- | A non-validating XML parser.  For the input grammar, see
--   <http://www.w3.org/TR/REC-xml>.
module Text.Xml.HaXml.Parse
  (
  -- * Parse a whole document
    xmlParse
  -- * Parse just a DTD
  , dtdParse
  ) where

-- An XML parser, written using a slightly extended version of the
-- Hutton/Meijer parser combinators.  The input is tokenised internally
-- by the lexer xmlLex.  Whilst parsing, we gather a symbol
-- table of entity references.  PERefs must be defined before use, so we
-- expand their uses as we encounter them, forcing the remainder of the
-- input to be re-lexed and re-parsed.  GERefs are simply stored for
-- later retrieval.

import Prelude hiding (either,maybe,sequence)
import Maybe hiding (maybe)
import Char (isSpace)
import Text.Xml.HaXml.Types
import Text.Xml.HaXml.Lex
import Text.ParserCombinators.HuttonMeijerWallace
import Monad hiding (sequence)


#if defined(__NHC__)
import IOExtras (unsafePerformIO)
#endif
#if defined(__HBC__)
import UnsafePerformIO
#endif
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
import IOExts (unsafePerformIO)
#endif

--  #define DEBUG

#if defined(DEBUG)
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
import IOExts(trace)
#else
import NonStdTrace
#endif
#else
trace _ x =  x
#endif


-- | To parse a whole document, @xmlParse file content@ takes a filename
--   (for error reports) and the string content of that file.
xmlParse :: String -> String -> Document

-- | To parse just a DTD, @dtdParse file content@ takes a filename
--   (for error reports) and the string content of that file.  If no
--   DTD was found, you get Nothing rather than an error.
dtdParse :: String -> String -> Maybe DocTypeDecl

xmlParse name = sanitycheck . papply document emptySTs . xmlLex name
dtdParse name = sanitycheck . papply justDTD  emptySTs . xmlLex name

sanitycheck :: Show p => [(a,s,[(p,t)])] -> a
sanitycheck [] = error "***Error at line 0: document not XML?"
sanitycheck ((x,_,[]):_) = x
sanitycheck ((x,_,s@((n,_):_)):xs) =
  trace ("***Warning at "++show n++": data beyond end of parsed document") x


---- Symbol table stuff ----

type SymTabs = (SymTab PEDef, SymTab EntityDef)

emptySTs :: SymTabs
emptySTs = (emptyST, emptyST)

addPE :: String -> PEDef -> SymTabs -> SymTabs
addPE n v (pe,ge) = (addST n v pe, ge)

addGE :: String -> EntityDef -> SymTabs -> SymTabs
addGE n v (pe,ge) = let newge = addST n v ge in newge `seq` (pe, newge)

lookupPE :: String -> SymTabs -> Maybe PEDef
lookupPE s (pe,ge) = lookupST s pe

flattenEV :: EntityValue -> String
flattenEV (EntityValue evs) = concatMap flatten evs
  where
    flatten (EVString s)          = s
    flatten (EVRef (RefEntity r)) = "&" ++r++";"
    flatten (EVRef (RefChar r))   = "&#"++r++";"


---- Misc ----

fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a


---- Auxiliary Parsing Functions ----

name :: Parser SymTabs Token Name
name = do {(p,TokName s) <- item; return s}

string, freetext :: Parser SymTabs Token String
string   = do {(p,TokName s) <- item; return s}
freetext = do {(p,TokFreeText s) <- item; return s}

maybe :: Parser SymTabs Token a -> Parser SymTabs Token (Maybe a)
maybe p =
    ( p >>= return . Just) +++
    ( return Nothing)

either :: Parser SymTabs Token a -> Parser SymTabs Token b
          -> Parser SymTabs Token (Either a b)
either p q =
    ( p >>= return . Left) +++
    ( q >>= return . Right)

word :: String -> Parser SymTabs Token ()
word s = P (\st inp-> case inp of {
                       ((p,TokName n):ts) -> if s==n then [((),st,ts)]
                                             else [];
                       ((p,TokFreeText n):ts) -> if s==n then [((),st,ts)]
                                                 else [];
                       ts -> [] } )

posn :: Parser SymTabs Token Posn
posn = P (\st inp-> case inp of {
                     ((p,_):_) -> [(p,st,inp)];
                     [] -> []; } )

nmtoken :: Parser SymTabs Token NmToken
nmtoken = (string +++ freetext)


peRef :: Parser SymTabs Token a -> Parser SymTabs Token a
peRef p =
    p +++
    ( do pn <- posn
         n <- pereference
         tr <- trace ("Looking up %"++n++"\n") $ stquery (lookupPE n)
         case tr of
           (Just (PEDefEntityValue ev)) ->
                      do trace ("defn:  "++flattenEV ev++"\n") $
                           reparse (xmlReLex (posInNewCxt ("macro %"++n++";")
                                                          (Just pn))
                                             (flattenEV ev))
                         peRef p
           (Just (PEDefExternalID (PUBLIC _ (SystemLiteral f)))) ->
                      do let val = unsafePerformIO (readFile f)
                         trace ("reading from file "++f++"\n") $
                           reparse (xmlReLex (posInNewCxt ("file "++f)
                                                          (Just pn)) val)
                         peRef p
           (Just (PEDefExternalID (SYSTEM eid))) ->
                      do peRef p
           Nothing -> mzero `elserror` "PEReference use before definition" )

blank :: Parser SymTabs Token a -> Parser SymTabs Token a
blank p =
    p +++
    ( do n <- pereference
         tr <- trace ("Looking up %"++n++"\n") $ stquery (lookupPE n)
         case tr of
           (Just (PEDefEntityValue ev))
                    | all isSpace (flattenEV ev)  ->
                            do trace ("Empty macro definition\n") $ blank p
           (Just _) -> mzero
           Nothing  -> mzero `elserror` "PEReference use before definition" )
         


---- XML Parsing Functions ----

justDTD :: Parser SymTabs Token (Maybe DocTypeDecl)
justDTD =
  ( do (ExtSubset _ ds) <- trace ("Trying external subset\n") $
                           extsubset
       if null ds then mzero
         else return (Just (DTD "extsubset" Nothing (concatMap extract ds)))
  ) +++
  ( do (Prolog _ dtd) <- prolog
       return dtd )
 where extract (ExtMarkupDecl m) = [m]
       extract (ExtConditionalSect (IncludeSect i)) = concatMap extract i
       extract (ExtConditionalSect (IgnoreSect i)) = []

document :: Parser SymTabs Token Document
document = do
    p <- prolog `elserror` "unrecognisable XML prolog"
    e <- element `elserror` "no toplevel document element"
    ms <- many misc
    (_,ge) <- stget
    return (Document p ge e)

comment :: Parser SymTabs Token Comment
comment = do
    bracket (tok TokCommentOpen) freetext (tok TokCommentClose)

processinginstruction :: Parser SymTabs Token ProcessingInstruction
processinginstruction = do
    tok TokPIOpen
    n <- string  `elserror` "processing instruction has no target"
    f <- freetext
    tok TokPIClose `elserror` "missing ?>"
    return (n, f)

cdsect :: Parser SymTabs Token CDSect
cdsect = do
    tok TokSectionOpen
    bracket (tok (TokSection CDATAx)) chardata (tok TokSectionClose)

prolog :: Parser SymTabs Token Prolog
prolog = do
    x <- maybe xmldecl
    many misc
    dtd <- maybe doctypedecl
    many misc
    return (Prolog x dtd)

xmldecl :: Parser SymTabs Token XMLDecl
xmldecl = do
    tok TokPIOpen
    (word "xml" +++ word "XML")
    p <- posn
    s <- freetext
    tok TokPIClose `elserror` "missing ?> in <?xml ...?>"
    return ((fst3 . head . papply aux emptySTs . xmlReLex p) s)
  where
    aux = do
      v <- versioninfo  `elserror` "missing XML version info"
      e <- maybe encodingdecl
      s <- maybe sddecl
      return (XMLDecl v e s)

versioninfo :: Parser SymTabs Token VersionInfo
versioninfo = do
    (word "version" +++ word "VERSION")
    tok TokEqual
    bracket (tok TokQuote) freetext (tok TokQuote)

misc :: Parser SymTabs Token Misc
misc = 
    ( comment >>= return . Comment) +++
    ( processinginstruction >>= return . PI)

doctypedecl :: Parser SymTabs Token DocTypeDecl
doctypedecl = do
    tok TokSpecialOpen
    tok (TokSpecial DOCTYPEx)
    n <- name
    eid <- maybe externalid
    es <- maybe (bracket (tok TokSqOpen)
                         (many (peRef markupdecl))
                         (tok TokSqClose))
    blank (tok TokAnyClose)  `elserror` "missing > in DOCTYPE decl"
    return (DTD n eid (case es of { Nothing -> []; Just e -> e }))

markupdecl :: Parser SymTabs Token MarkupDecl
markupdecl =
    ( elementdecl >>= return . Element) +++
    ( attlistdecl >>= return . AttList) +++
    ( entitydecl >>= return . Entity) +++
    ( notationdecl >>= return . Notation) +++
    ( misc >>= return . MarkupMisc)

extsubset :: Parser SymTabs Token ExtSubset
extsubset = do
    td <- maybe textdecl
    ds <- many (peRef extsubsetdecl)
    return (ExtSubset td ds)

extsubsetdecl :: Parser SymTabs Token ExtSubsetDecl
extsubsetdecl =
    ( markupdecl >>= return . ExtMarkupDecl) +++
    ( conditionalsect >>= return . ExtConditionalSect)

sddecl :: Parser SymTabs Token SDDecl
sddecl = do
    (word "standalone" +++ word "STANDALONE")
    tok TokEqual `elserror` "missing = in 'standalone' decl"
    bracket (tok TokQuote)
            ( (word "yes" >> return True) +++
              (word "no" >> return False) `elserror`
              "'standalone' decl requires 'yes' or 'no' value" )
            (tok TokQuote)

element :: Parser SymTabs Token Element
element = do
    tok TokAnyOpen
    (ElemTag n as) <- elemtag
    (( do tok TokEndClose
          return (Elem n as [])) +++
     ( do tok TokAnyClose
          cs <- many content
          p <- posn
          m <- bracket (tok TokEndOpen) name (tok TokAnyClose)
          checkmatch p n m
          return (Elem n as cs))
     `elserror` "missing > or /> in element tag")

checkmatch :: Posn -> Name -> Name -> Parser SymTabs Token ()
checkmatch p n m =
  if n == m then return ()
  else error ("Error in  "++show p++"\n  tag <"++n++"> terminated by </"++m++">")

elemtag :: Parser SymTabs Token ElemTag
elemtag = do
    n <- name `elserror` "malformed element tag"
    as <- many attribute
    return (ElemTag n as)

attribute :: Parser SymTabs Token Attribute
attribute = do
    n <- name
    tok TokEqual `elserror` "missing = in attribute"
    v <- attvalue `elserror` "missing attvalue"
    return (n,v)

content :: Parser SymTabs Token Content
content =
    ( element >>= return . CElem) +++
    ( chardata >>= return . CString False) +++
    ( reference >>= return . CRef) +++
    ( cdsect >>= return . CString True) +++
    ( misc >>= return . CMisc)

elementdecl :: Parser SymTabs Token ElementDecl
elementdecl = do
    tok TokSpecialOpen
    tok (TokSpecial ELEMENTx)
    n <- peRef name `elserror` "missing identifier in ELEMENT decl"
    c <- peRef contentspec `elserror` "missing content spec in ELEMENT decl"
    blank (tok TokAnyClose) `elserror` "expected > terminating ELEMENT decl"
    return (ElementDecl n c)

contentspec :: Parser SymTabs Token ContentSpec
contentspec =
    ( peRef (word "EMPTY") >> return EMPTY) +++
    ( peRef (word "ANY") >> return ANY) +++
    ( peRef mixed >>= return . Mixed) +++
    ( peRef cp >>= return . ContentSpec)

choice :: Parser SymTabs Token [CP]
choice = do
    bracket (tok TokBraOpen)
            (peRef cp `sepby1` (tok TokPipe))
            (blank (tok TokBraClose))

sequence :: Parser SymTabs Token [CP]
sequence = do
    bracket (tok TokBraOpen)
            (peRef cp `sepby1` (tok TokComma))
            (blank (tok TokBraClose))

cp :: Parser SymTabs Token CP
cp =
    ( do n <- name
         m <- modifier
         return (TagName n m)) +++
    ( do ss <- sequence
         m <- modifier
         return (Seq ss m)) +++
    ( do cs <- choice
         m <- modifier
         return (Choice cs m))

modifier :: Parser SymTabs Token Modifier
modifier =
    ( tok TokStar >> return Star) +++
    ( tok TokQuery >> return Query) +++
    ( tok TokPlus >> return Plus) +++
    ( return None)

mixed :: Parser SymTabs Token Mixed
mixed = do
    tok TokBraOpen
    peRef (do tok TokHash
              word "PCDATA")
    cont
  where
    cont = ( do cs <- many (peRef (do tok TokPipe
                                      peRef name))
                blank (tok TokBraClose >> tok TokStar)
                return (PCDATAplus cs)) +++
           ( blank (tok TokBraClose >> tok TokStar) >> return PCDATA) +++
           ( blank (tok TokBraClose) >> return PCDATA)

attlistdecl :: Parser SymTabs Token AttListDecl
attlistdecl = do
    tok TokSpecialOpen
    tok (TokSpecial ATTLISTx)
    n <- peRef name `elserror` "missing identifier in ATTLIST"
    ds <- peRef (many (peRef attdef))
    blank (tok TokAnyClose) `elserror` "missing > terminating ATTLIST"
    return (AttListDecl n ds)

attdef :: Parser SymTabs Token AttDef
attdef =
  do n <- peRef name
     t <- peRef atttype `elserror` "missing attribute type in attlist defn"
     d <- peRef defaultdecl
     return (AttDef n t d)

atttype :: Parser SymTabs Token AttType
atttype =
    ( word "CDATA" >> return StringType) +++
    ( tokenizedtype >>= return . TokenizedType) +++
    ( enumeratedtype >>= return . EnumeratedType)

tokenizedtype :: Parser SymTabs Token TokenizedType
tokenizedtype =
    ( word "ID" >> return ID) +++
    ( word "IDREF" >> return IDREF) +++
    ( word "IDREFS" >> return IDREFS) +++
    ( word "ENTITY" >> return ENTITY) +++
    ( word "ENTITIES" >> return ENTITIES) +++
    ( word "NMTOKEN" >> return NMTOKEN) +++
    ( word "NMTOKENS" >> return NMTOKENS)

enumeratedtype :: Parser SymTabs Token EnumeratedType
enumeratedtype =
    ( notationtype >>= return . NotationType) +++
    ( enumeration >>= return . Enumeration)

notationtype :: Parser SymTabs Token NotationType
notationtype = do
    word "NOTATION"
    bracket (tok TokBraOpen)
            (name `sepby1` (tok TokPipe))
            (tok TokBraClose)

enumeration :: Parser SymTabs Token Enumeration
enumeration =
    bracket (tok TokBraOpen)
            (peRef nmtoken `sepby1` peRef ((tok TokPipe)))
            (blank (tok TokBraClose))

defaultdecl :: Parser SymTabs Token DefaultDecl
defaultdecl =
    ( tok TokHash >> word "REQUIRED" >> return REQUIRED) +++
    ( tok TokHash >> word "IMPLIED" >> return IMPLIED) +++
    ( do f <- maybe (tok TokHash >> word "FIXED" >> return FIXED)
         a <- peRef attvalue
         return (DefaultTo a f))

conditionalsect :: Parser SymTabs Token ConditionalSect
conditionalsect =
    ( do tok TokSectionOpen
         peRef (tok (TokSection INCLUDEx))
         tok TokSqOpen `elserror` "missing [ after INCLUDE"
         i <- many (peRef extsubsetdecl)
         tok TokSectionClose `elserror` "missing ]]> for INCLUDE section"
         return (IncludeSect i)) +++
    ( do tok TokSectionOpen
         peRef (tok (TokSection IGNOREx))
         tok TokSqOpen `elserror` "missing [ after IGNORE"
         i <- many ignoresectcontents
         tok TokSectionClose `elserror` "missing ]]> for IGNORE section"
         return (IgnoreSect i))

ignoresectcontents :: Parser SymTabs Token IgnoreSectContents
ignoresectcontents = do
    i <- ignore
    is <- many (do tok TokSectionOpen
                   ic <- ignoresectcontents
                   tok TokSectionClose
                   ig <- ignore
                   return (ic,ig))
    return (IgnoreSectContents i is)

ignore :: Parser SymTabs Token Ignore
ignore = do
  is <- many1 (nottok [TokSectionOpen,TokSectionClose])
  trace ("ignored all of: "++show is++"\n") $ return Ignore

reference :: Parser SymTabs Token Reference
reference =
    ( entityref >>= return . RefEntity) +++
    ( charref >>= return . RefChar)

entityref :: Parser SymTabs Token EntityRef
entityref = do
    bracket (tok TokAmp) name (tok TokSemi)

charref :: Parser SymTabs Token CharRef
charref = do
    bracket (tok TokAmp) freetext (tok TokSemi)

pereference :: Parser SymTabs Token PEReference
pereference = do
    bracket (tok TokPercent) nmtoken (tok TokSemi)

entitydecl :: Parser SymTabs Token EntityDecl
entitydecl =
    ( gedecl >>= return . EntityGEDecl) +++
    ( pedecl >>= return . EntityPEDecl)

gedecl :: Parser SymTabs Token GEDecl
gedecl = do
    tok TokSpecialOpen
    tok (TokSpecial ENTITYx)
    n <- name
    e <- entitydef `elserror` "missing entity defn in G ENTITY decl"
    tok TokAnyClose `elserror` "expected > terminating G ENTITY decl"
    stupd (addGE n e)
    return (GEDecl n e)

pedecl :: Parser SymTabs Token PEDecl
pedecl = do
    tok TokSpecialOpen
    tok (TokSpecial ENTITYx)
    tok TokPercent
    n <- name
    e <- pedef `elserror` "missing entity defn in P ENTITY decl"
    tok TokAnyClose `elserror` "expected > terminating P ENTITY decl"
    stupd (addPE n e)
    return (PEDecl n e)

entitydef :: Parser SymTabs Token EntityDef
entitydef =
    ( entityvalue >>= return . DefEntityValue) +++
    ( do eid <- externalid
         ndd <- maybe ndatadecl
         return (DefExternalID eid ndd))

pedef :: Parser SymTabs Token PEDef
pedef =
    ( entityvalue >>= return . PEDefEntityValue) +++
    ( externalid >>= return . PEDefExternalID)

externalid :: Parser SymTabs Token ExternalID
externalid =
    ( do word "SYSTEM"
         s <- systemliteral
         return (SYSTEM s)) +++
    ( do word "PUBLIC"
         p <- pubidliteral
         s <- systemliteral
         return (PUBLIC p s))

ndatadecl :: Parser SymTabs Token NDataDecl
ndatadecl = do
    word "NDATA"
    n <- name
    return (NDATA n)

textdecl :: Parser SymTabs Token TextDecl
textdecl = do
    tok TokPIOpen
    (word "xml" +++ word "XML")
    v <- maybe versioninfo
    e <- encodingdecl
    tok TokPIClose `elserror` "expected ?> terminating text decl"
    return (TextDecl v e)

extparsedent :: Parser SymTabs Token ExtParsedEnt
extparsedent = do
    t <- maybe textdecl
    c <- content
    return (ExtParsedEnt t c)

extpe :: Parser SymTabs Token ExtPE
extpe = do
    t <- maybe textdecl
    e <- many (peRef extsubsetdecl)
    return (ExtPE t e)

encodingdecl :: Parser SymTabs Token EncodingDecl
encodingdecl = do
    (word "encoding" +++ word "ENCODING")
    tok TokEqual `elserror` "expected = in 'encoding' decl"
    f <- bracket (tok TokQuote) freetext (tok TokQuote)
    return (EncodingDecl f)

notationdecl :: Parser SymTabs Token NotationDecl
notationdecl = do
    tok TokSpecialOpen
    tok (TokSpecial NOTATIONx)
    n <- name
    e <- either externalid publicid
    tok TokAnyClose `elserror` "expected > terminating NOTATION decl"
    return (NOTATION n e)

publicid :: Parser SymTabs Token PublicID
publicid = do
    word "PUBLIC"
    p <- pubidliteral
    return (PUBLICID p)

entityvalue :: Parser SymTabs Token EntityValue
entityvalue = do
    evs <- bracket (tok TokQuote) (many (peRef ev)) (tok TokQuote)
    return (EntityValue evs)

ev :: Parser SymTabs Token EV
ev =
    ( freetext >>= return . EVString) +++
    ( reference >>= return . EVRef)

attvalue :: Parser SymTabs Token AttValue
attvalue = do
    avs <- bracket (tok TokQuote)
                   (many (either freetext reference))
                   (tok TokQuote)
    return (AttValue avs)

systemliteral :: Parser SymTabs Token SystemLiteral
systemliteral = do
    s <- bracket (tok TokQuote) freetext (tok TokQuote)
    return (SystemLiteral s)		-- note: need to fold &...; escapes

pubidliteral :: Parser SymTabs Token PubidLiteral
pubidliteral = do
    s <- bracket (tok TokQuote) freetext (tok TokQuote)
    return (PubidLiteral s)		-- note: need to fold &...; escapes

chardata :: Parser SymTabs Token CharData
chardata = freetext

