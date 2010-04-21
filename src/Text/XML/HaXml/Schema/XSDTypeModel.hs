module Text.XML.HaXml.Schema.XSDTypeModel
  ( module Text.XML.HaXml.Schema.XSDTypeModel
  ) where

import Text.XML.HaXml.Types      (Name,Namespace)

data Schema        = Schema [SchemaItem] Annotation QForm QForm (Maybe Final)
                            (Maybe TargetNamespace) (Maybe Block)
data SchemaItem    = Simple  SimpleType  Annotation
                   | Complex ComplexType Annotation (Maybe Final)
                   | Decl    ElementDecl Annotation
                   | Include SchemaLocation
                   | Redefine SchemaLocation ComplexType
                   | Substitution ElementDecl Name
                   | Abstract     ElementDecl
                   | Constraint Name Constraint
                   | Import  Namespace

data SimpleType    = Primitive       PrimitiveType
                   | Restricted Name SimpleType   Restriction Fixed
                   | ListOf     Name SimpleType   Restriction Fixed
                   | UnionOf    Name [SimpleType] Restriction Fixed
data ComplexType   = ComplexType (Maybe Name) [AttributeDecl] ElementsDecl
                                 Mixed (Maybe Block)
                   | SimpleContent (Maybe Name) [AttributeDecl] SimpleType
                   | ComplexExtension (Maybe Name) Name ElementsDecl
                   | ComplexRestriction (Maybe Name) Name ElementsDecl
data Group         = Group Name ElementsDecl

data ElementsDecl  = Sequence  [ElementDecl]
                   | Unordered [ElementDecl]
data AttributeGroup= AttributeGroup [AttributeDecl]

data ElementDecl   = ElementDecl Name (Either SimpleType ComplexType) Occurs
                                      DefaultValue Nillable
                                      {-Annotation-} QForm
                   | ElementRef  Name Occurs
                   | GroupRef    Name
                   | Choice      [ElementDecl]
data AttributeDecl = AttributeDecl Name SimpleType Use
                                   (Maybe (Either DefaultValue FixedValue))
                                   Annotation QForm
                   | AttributeGroupRef Name


data Occurs        = Occurs (Maybe Int) (Maybe Int)
data Use           = Required | Optional | Prohibited
                     -- (1,1) |   (0,1)  |   (0,0) -- corresp. to Occurs values

data PrimitiveType = String | Decimal | Boolean
                   | AnyURI | QName | Notation
                   | Base64Binary | HexBinary | Float | Double
                   | Duration | DateTime | Date | Time
                   | GYearMonth | GYear | GMonthDay | GDay | GMonth
               

data Restriction   = Range Occurs
                   | Pattern Regexp
                   | Enumeration [String]
type Mixed         = Bool
type Nillable      = Bool
type Fixed         = Bool

data Annotation    = Documentation String
                   | AppInfo String
                   | NoAnnotation

data QForm         = Qualified | Unqualified -- only matters for locally decl'd
type TargetNamespace
                   = URI
data Final         = NoExtension | NoRestriction | AllFinal
type Block         = Final

data Constraint    = Unique Selector [Field]
                   | Key    Selector [Field]
                   | KeyRef Selector [Field]
type Selector      = String	-- XPath query for scope of constraint
type Field         = String	-- XPath query for entity being constrained

-- check all of the following.
type SchemaLocation= String
type DefaultValue  = String
type FixedValue    = String
type Regexp        = String
type URI           = String
