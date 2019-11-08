module AST where

data TOP =
    Class |
    Inst |
    Defun Fun |
    Import
    deriving (Show)

data Position = Pos Int Int deriving (Show)

-- function definition
data Fun = Fun Position String [Arg] (Maybe QType) [Pred] [Expr] deriving (Show)

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
    ExprIndex Expr Expr
    deriving (Show)

data Literal =
    LitStr String |
    LitChar Char |
    LitInt Integer |
    LitFloat Double
    deriving (Show)

data DBind =
    DBindDotID Position [String] (Maybe [DBind]) |
    DBindIgnore Position |
    DBindTuple Position [DBind]
    deriving (Show)
