module Text.XML.HaXml.Schema.XSDTypeModel
  ( module Text.XML.HaXml.Schema.XSDTypeModel
  ) where

import Text.XML.HaXml.Types      (Name,Namespace,QName)

data Schema        = Schema Annotation QForm QForm (Maybe Final) (Maybe Block)
                            (Maybe TargetNamespace) [SchemaItem]
                     deriving (Eq,Show)
data SchemaItem    = Simple  SimpleType  Annotation
                   | Complex ComplexType Annotation (Maybe Final)
                   | Decl    ElementDecl Annotation
                   | Include SchemaLocation
                   | Redefine SchemaLocation ComplexType
                   | Substitution ElementDecl Name
                   | Abstract     ElementDecl
                   | Constraint Name Constraint
                   | Import  Namespace
                     deriving (Eq,Show)

data SimpleType    = Primitive       PrimitiveType
                   | Restricted Name SimpleType   Restriction Fixed
                   | ListOf     Name SimpleType   Restriction Fixed
                   | UnionOf    Name [SimpleType] Restriction Fixed
                     deriving (Eq,Show)
data ComplexType   = ComplexType (Maybe Name) [AttributeDecl] ElementsDecl
                                 Mixed (Maybe Block)
                   | SimpleContent (Maybe Name) [AttributeDecl] SimpleType
                   | ComplexExtension (Maybe Name) Name ElementsDecl
                   | ComplexRestriction (Maybe Name) Name ElementsDecl
                     deriving (Eq,Show)
data Group         = Group Name ElementsDecl
                     deriving (Eq,Show)

data ElementsDecl  = Sequence  [ElementDecl]
                   | Unordered [ElementDecl]
                     deriving (Eq,Show)
data AttributeGroup= AttributeGroup [AttributeDecl]
                     deriving (Eq,Show)

data ElementDecl   = ElementDecl Name (Either QName
                                              (Either SimpleType ComplexType))
                                      Occurs
                                      DefaultValue Nillable
                                      {-Annotation-} QForm
                   | ElementRef  QName Occurs
                   | GroupRef    QName
                   | Choice      [ElementDecl]
                     deriving (Eq,Show)
data AttributeDecl = AttributeDecl Name (Either QName SimpleType) Use
                                   (Maybe (Either DefaultValue FixedValue))
                                   Annotation QForm
                   | AttributeGroupRef QName
                     deriving (Eq,Show)


data Occurs        = Occurs (Maybe Int) (Maybe Int)
                     deriving (Eq,Show)
data Use           = Required | Optional | Prohibited
                     -- (1,1) |   (0,1)  |   (0,0) -- corresp. to Occurs values
                     deriving (Eq,Show)

data PrimitiveType = String | Boolean | Decimal | Float | Double
                   | Duration | DateTime | Time | Date
                   | GYearMonth | GYear | GMonthDay | GDay | GMonth
                   | Base64Binary | HexBinary
                   | AnyURI | QName | Notation
                     deriving (Eq,Show)
               

data Restriction   = Range Occurs
                   | Pattern Regexp
                   | Enumeration [String]
                     deriving (Eq,Show)
type Mixed         = Bool
type Nillable      = Bool
type Fixed         = Bool

data Annotation    = Documentation String
                   | AppInfo String
                   | NoAnnotation
                     deriving (Eq,Show)

data QForm         = Qualified | Unqualified -- only matters for locally decl'd
                     deriving (Eq,Show)
type TargetNamespace
                   = URI
data Final         = NoExtension | NoRestriction | AllFinal
                     deriving (Eq,Show)
type Block         = Final

data Constraint    = Unique Selector [Field]
                   | Key    Selector [Field]
                   | KeyRef Selector [Field]
                     deriving (Eq,Show)
type Selector      = String	-- XPath query for scope of constraint
type Field         = String	-- XPath query for entity being constrained

-- check all of the following.
type SchemaLocation= String
type DefaultValue  = String
type FixedValue    = String
type Regexp        = String
type URI           = String
type TypeName      = String
