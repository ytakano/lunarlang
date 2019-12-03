module AST where

import qualified System.FilePath as FP

data TOP =
    TOPClassDef ClassDef |
    TOPInstance Instance |
    TOPDefun Defun |
    TOPData Data |
    TOPImport Import |
    TOPStruct Struct
    deriving (Show)

data IDPos = IDPos {
    idPosPos :: Position,
    idPosID  :: String
} deriving (Show)

data ClassDef = ClassDef {
    classPos  :: Position,
    classID   :: IDPos,
    classTVK  :: [TypeVarKind],
    classPred :: [Pred],
    classIF   :: [Interface]
} deriving (Show)

data Instance = Instance {
    instPos   :: Position,
    instHead  :: Pred,
    instPred  :: [Pred],
    instDefun :: [Defun]
} deriving (Show)

data Defun    = Defun {
    defunPos  :: Position,
    defunID   :: IDPos,
    defunArgs :: [Arg],
    defunRet  :: Maybe QType,
    defunPred :: [Pred],
    defunExpr :: [Expr]
} deriving (Show)

data Data     = Data {
    dataPos  :: Position,
    dataID   :: IDPos,
    dataTVK  :: [TypeVarKind],
    dataPred :: [Pred],
    dataMem  :: [SumMem]
} deriving (Show)

data Import   = Import Position [String] HereAs deriving (Show)

data Struct   = Struct {
    structPos  :: Position,
    structID   :: IDPos,
    structTVK  :: [TypeVarKind],
    structPred :: [Pred],
    structMem  :: [ProdMem]
} deriving (Show)

data Position = Pos Int Int deriving (Show)

-- argument
data Arg = Arg {
    argPos   :: Position,
    argDBind :: DBind,
    argQType :: Maybe QType
} deriving (Show)

-- qualifier
data Qual =
    Shared |             -- shared type
    Uniq deriving (Show, Eq) -- unique type

-- qualified type
data QType = QType {
    qtypePos  :: Position,
    qtypeQual :: Maybe Qual,
    qtypeType :: LType
 } deriving (Show)

-- type of Lunar language
data LType =
    IDType ID [QType] |
    TVar Position String [QType] |
    ArrayType Position QType (Maybe Expr) |
    TupleType Position [QType] |
    FuncType Position [QType] QType |
    VoidType
    deriving (Show)

data ID =
    IDRel {
        idRelPos :: Position,
        idRelID  :: [String]
    } |
    IDAbs {
        idAbsPos  :: Position,
        idAbsOrig :: [String],
        idAbsFile :: Maybe FP.FilePath,
        idAbsID   :: String
    } deriving (Show)

-- predicate
data Pred = Pred {
    predPos :: Position,
    predID  :: ID,
    predArg :: QType
 } deriving (Show)

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
data SumMem  = SumMem {
    sumMemPos   :: Position,
    sumMemID    :: String,
    sumMemQType :: [QType]
} deriving (Show)

data Kind =
    KV Int |
    KStar |
    KArray Kind Kind
    deriving (Show, Eq)

data TypeVarKind = TypeVarKind Position String (Maybe Kind) deriving (Show)

data IntName =
    IntFunc Position String |
    IntInfix Position String |
    IntPrefix Position String
    deriving (Show)

data Interface = Interface {
    ifName :: [IntName],
    ifType :: LType
} deriving (Show)

data HereAs =
    ImportNS |
    ImportHere |
    ImportAs [String]
    deriving (Show)

data ProdMem = ProdMem {
    prodMemPos   :: Position,
    prodMemID    :: String,
    prodMemQType :: QType
} deriving (Show)
