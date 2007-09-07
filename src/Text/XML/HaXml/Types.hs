{- |
   This module defines an internal (generic) representation for XML
   documents including their DTDs.

   History:
   The original module was derived by hand from the XML specification,
   following the grammar precisely.  Then we simplified the types,
   removing layers of indirection and redundancy, and generally making
   things easier to work with.  Then we allowed PEReferences to be
   ubiquitous, by removing them from the types and resolving all
   PE references at parse-time.  Finally, we added a per-document
   symbol table for GEReferences, and a whitespace-significance flag
   for plaintext.
-}

module Text.XML.HaXml.Types
  (
  -- * A simple symbol table mapping strings (references) to values.
    SymTab
  -- ** Symbol table operations
  , emptyST
  , addST
  , lookupST

  -- * XML Types
  -- ** The top-level document container
  , Document(..)

  -- ** The main document content
  , Element(..)
  , ElemTag(..)
  , Content(..)
  , Attribute
  , AttValue(..)
  , info

  -- ** Administrative parts of the document
  , Prolog(..)
  , XMLDecl(..)
  , Misc(..)
  , ProcessingInstruction
  , SDDecl
  , VersionInfo
  , Comment
  , PITarget

  -- ** The DTD
  -- *** content model
  , DocTypeDecl(..)
  , MarkupDecl(..)
  , ExtSubset(..)
  , ExtSubsetDecl(..)
  , ElementDecl(..)
  , ContentSpec(..)
  , CP(..)
  , Modifier(..)
  , Mixed(..)

  -- *** attribute model
  , AttListDecl(..)
  , AttDef(..)
  , AttType(..)
  , TokenizedType(..)
  , EnumeratedType(..)
  , NotationType
  , Enumeration
  , DefaultDecl(..)
  , FIXED(..)

  -- *** conditional sections
  , ConditionalSect(..)
  , IncludeSect
  , IgnoreSect
  , Ignore(..)
  , IgnoreSectContents(..)

  -- ** References
  , Reference(..)
  , EntityRef
  , CharRef
  , PEReference

  -- ** Entities
  , EntityDecl(..)
  , GEDecl(..)
  , PEDecl(..)
  , EntityDef(..)
  , PEDef(..)
  , ExternalID(..)
  , NDataDecl(..)
  , TextDecl(..)
  , ExtParsedEnt(..)
  , ExtPE(..)
  , NotationDecl(..)
  , PublicID(..)
  , EncodingDecl(..)
  , EntityValue(..)
  , EV(..)
  , PubidLiteral(..)
  , SystemLiteral(..)

  -- ** Basic value types
  , Name
  , Names
  , NmToken
  , NmTokens
  , CharData
  , CDSect
  ) where


{- A simple symbol table for storing macros whilst parsing. -}

type SymTab a = [(String,a)]

emptyST :: SymTab a
emptyST  = []

addST :: String -> a -> SymTab a -> SymTab a
addST n v = ((n,v):)

lookupST :: String -> SymTab a -> Maybe a
lookupST = lookup



{- XML types start here -}

-- | The symbol table stored in a document holds all its general entity
--   reference definitions.
data Document i = Document Prolog (SymTab EntityDef) (Element i) [Misc]
                  deriving Eq
data Prolog     = Prolog (Maybe XMLDecl) [Misc] (Maybe DocTypeDecl) [Misc]
                  deriving Eq
data XMLDecl    = XMLDecl VersionInfo (Maybe EncodingDecl) (Maybe SDDecl)
                  deriving Eq
data Misc       = Comment Comment
                | PI ProcessingInstruction 
                deriving Eq
              
type ProcessingInstruction = (PITarget,String)

type SDDecl      = Bool 
type VersionInfo = String 
type Comment     = String 
type PITarget    = String 

data DocTypeDecl = DTD Name (Maybe ExternalID) [MarkupDecl]  deriving Eq
data MarkupDecl  = Element  ElementDecl
                 | AttList  AttListDecl
                 | Entity   EntityDecl
                 | Notation NotationDecl
                 | MarkupMisc Misc
                 deriving Eq

data ExtSubset     = ExtSubset (Maybe TextDecl) [ExtSubsetDecl]  deriving Eq
data ExtSubsetDecl = ExtMarkupDecl MarkupDecl
                   | ExtConditionalSect ConditionalSect
                   deriving Eq

data Element i = Elem Name [Attribute] [Content i] deriving Eq
                                       	-- ^ intermediate for parsing
data ElemTag   = ElemTag Name [Attribute]
type Attribute = (Name, AttValue)
data Content i = CElem (Element i) i
               | CString Bool CharData i
			-- ^ bool is whether whitespace is significant
               | CRef Reference i
               | CMisc Misc i
               deriving Eq
info (CElem _ i) = i
info (CString _ _ i) = i
info (CRef _ i) = i
info (CMisc _ i) = i

instance Functor Document where
  fmap f (Document p st e ms) = Document p st (fmap f e) ms
instance Functor Element where
  fmap f (Elem t as cs) = Elem t as (map (fmap f) cs)
instance Functor Content where
  fmap f (CElem e i)     = CElem (fmap f e) (f i)
  fmap f (CString b s i) = CString b s (f i)
  fmap f (CRef r i)      = CRef r (f i)
  fmap f (CMisc m i)     = CMisc m (f i)

data ElementDecl = ElementDecl Name ContentSpec deriving Eq
data ContentSpec = EMPTY
                 | ANY
                 | Mixed Mixed
                 | ContentSpec CP
                 deriving Eq
-- FIXME: What is TagName here? Seems to be in disagreement with XML spec.
data CP = TagName Name Modifier
        | Choice [CP] Modifier
        | Seq [CP] Modifier 
        deriving Eq
data Modifier = None  -- ^ Just One
              | Query -- ^ Zero Or One
              | Star  -- ^ Zero Or More
              | Plus  -- ^ One Or More 
              deriving Eq
data Mixed = PCDATA
           | PCDATAplus [Name] 
           deriving Eq
data AttListDecl = AttListDecl Name [AttDef] deriving Eq
data AttDef      = AttDef Name AttType DefaultDecl deriving Eq
data AttType     = StringType
                 | TokenizedType TokenizedType
                 | EnumeratedType EnumeratedType 
                 deriving Eq
data TokenizedType = ID
                   | IDREF
                   | IDREFS
                   | ENTITY
                   | ENTITIES
                   | NMTOKEN
                   | NMTOKENS 
                   deriving Eq
data EnumeratedType = NotationType NotationType
                    | Enumeration Enumeration 
                    deriving Eq
type NotationType   = [Name]	-- nonempty list
type Enumeration    = [NmToken]	-- nonempty list
data DefaultDecl    = REQUIRED
                    | IMPLIED
                    | DefaultTo AttValue (Maybe FIXED) 
                    deriving Eq
data FIXED          = FIXED deriving Eq

data ConditionalSect = IncludeSect IncludeSect
                     | IgnoreSect IgnoreSect 
                     deriving Eq
type IncludeSect = [ExtSubsetDecl]
type IgnoreSect  = [IgnoreSectContents]
data Ignore      = Ignore deriving Eq
data IgnoreSectContents = IgnoreSectContents Ignore [(IgnoreSectContents,Ignore)]  deriving Eq

data Reference    = RefEntity EntityRef
                  | RefChar CharRef 
                  deriving (Eq,Show)
type EntityRef    = Name 
type CharRef      = Int
type PEReference  = Name 

data EntityDecl   = EntityGEDecl GEDecl
                  | EntityPEDecl PEDecl 
                  deriving Eq
data GEDecl       = GEDecl Name EntityDef deriving Eq
data PEDecl       = PEDecl Name PEDef deriving Eq
data EntityDef    = DefEntityValue EntityValue 
                  | DefExternalID ExternalID (Maybe NDataDecl) 
                  deriving Eq
data PEDef        = PEDefEntityValue EntityValue
                  | PEDefExternalID ExternalID deriving (Eq,Show)
data ExternalID   = SYSTEM SystemLiteral
                  | PUBLIC PubidLiteral SystemLiteral deriving (Eq,Show)
newtype NDataDecl = NDATA Name  deriving Eq

data TextDecl       = TextDecl (Maybe VersionInfo) EncodingDecl  deriving Eq
data ExtParsedEnt i = ExtParsedEnt (Maybe TextDecl) (Content i) deriving Eq
data ExtPE          = ExtPE (Maybe TextDecl) [ExtSubsetDecl] deriving Eq

data NotationDecl    = NOTATION Name (Either ExternalID PublicID) deriving Eq
newtype PublicID     = PUBLICID PubidLiteral deriving Eq
newtype EncodingDecl = EncodingDecl String deriving Eq

type Name     = String		 -- non-empty string
type Names    = [Name]		 -- non-empty list
type NmToken  = String		 -- non-empty string
type NmTokens = [NmToken]	 -- non-empty list

data AttValue    = AttValue [Either String Reference] deriving Eq
instance Show AttValue where
  show (AttValue v) = concatMap decode v
    where
      decode (Left  v)               = v
      decode (Right (RefEntity ent)) = "&"++ent++";"
      decode (Right (RefChar cref))  = "&"++show cref++";"

data EntityValue = EntityValue [EV] deriving (Eq,Show)
data EV = EVString String
 --  -- | EVPERef PEReference
        | EVRef Reference  deriving (Eq,Show)
newtype PubidLiteral  = PubidLiteral String deriving (Eq,Show)
newtype SystemLiteral = SystemLiteral String deriving (Eq,Show)
type CharData         = String 
type CDSect           = CharData

instance Eq ElemTag where
    (ElemTag n _) == (ElemTag m _)  = n==m
