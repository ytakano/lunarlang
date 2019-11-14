module AST where

data TOP =
    TOPClassDef ClassDef |
    TOPInstance Instance |
    TOPDefun Defun |
    TOPData Data |
    TOPImport Import |
    TOPStruct Struct
    deriving (Show)

data ClassDef = ClassDef Position String [TypeVarKind] [Pred] [Interface] deriving (Show)
data Instance = Instance Position Pred [Pred] [Defun] deriving (Show)
data Defun    = Defun Position String [Arg] (Maybe QType) [Pred] [Expr] deriving (Show)
data Data     = Data Position String [TypeVarKind] [Pred] [SumMem] deriving (Show)
data Import   = Import Position [String] HereAs deriving (Show)
data Struct   = Struct Position String [TypeVarKind] [Pred] [ProdMem] deriving (Show)

data Position = Pos Int Int deriving (Show)

-- argument
data Arg = Arg Position DBind (Maybe QType) deriving (Show)

-- qualifier
data Qual =
    Shared |             -- shared type
    Uniq deriving (Show) -- unique type

-- qualified type
data QType = QType Position (Maybe Qual) LType deriving (Show)

-- type of Lunar language
data LType =
    IDType Position [String] [QType] |
    TVar Position String [QType] |
    ArrayType Position QType |
    TupleType Position [QType] |
    FuncType Position [QType] QType |
    VoidType
    deriving (Show)

-- predicate
data Pred = Pred Position [String] QType deriving (Show)

data ElifElse =
    Elif Position Expr [Expr] ElifElse | Else Position [Expr] deriving (Show)

data Expr =
    ExprIf Position Expr [Expr] (Maybe ElifElse) |
    ExprPrefix String Expr |
    ExprBin String Expr Expr |
    ExprApply Expr [Expr] |
    ExprArray Position [Expr] |
    ExprTuple Position [Expr] |
    ExprDotID Position [String] |
    ExprLiteral Position Literal |
    ExprLet Position [(DBind, Maybe QType, Expr)] |
    Exprs Position [Expr] |
    ExprIndex Expr Expr |
    ExprMatch Position Expr [(Pattern, Expr)]
    deriving (Show)

data Literal =
    LitStr String |
    LitChar Char |
    LitInt Integer |
    LitFloat Double |
    LitBool Bool
    deriving (Show)

data DBind =
    DBindDotID Position [String] (Maybe [DBind]) |
    DBindTuple Position [DBind] |
    DBindIgnore Position
    deriving (Show)

data Pattern =
    PatDotID Position [String] (Maybe [Pattern]) |
    PatTuple Position [Pattern] |
    PatIgnore Position |
    PatLiteral Position Literal
    deriving (Show)

data TypeVar = TypeVar Position String deriving (Show)
data SumMem  = SumMem Position String [QType] deriving (Show)

data Kind =
    KStar |
    KArroy Kind Kind
    deriving (Show)

data TypeVarKind = TypeVarKind Position String (Maybe Kind) deriving (Show)

data IntName =
    IntFunc Position String |
    IntInfix Position String |
    IntPrefix Position String
    deriving (Show)

data Interface = Interface [IntName] LType deriving (Show)

data HereAs =
    ImportNS |
    ImportHere |
    ImportAs [String]
    deriving (Show)

data ProdMem = ProdMem Position String QType deriving (Show)
