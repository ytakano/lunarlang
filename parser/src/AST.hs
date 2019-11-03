module AST where

data TOP =
    Class |
    Inst |
    Defun Fun |
    Import
    deriving (Show)

-- function definition
data Fun = Fun String [Arg] (Maybe QType) [Pred] [Expr] deriving (Show)

-- argument
data Arg = Arg String (Maybe QType) deriving (Show)

-- qualifier
data Qual =
    Shared |             -- shared type
    Uniq deriving (Show) -- linear type

-- qualified type
data QType = QType (Maybe Qual) LType deriving (Show)

-- type of Lunar language
data LType =
    IDType [String] [QType] |
    TVar String [QType] |
    ArrayType QType |
    TupleType [QType]
    deriving (Show)

-- predicate
data Pred = Pred [String] QType deriving (Show)

data Expr =
    ExprIf Expr Expr Expr |
    ExprPrefix String Expr |
    ExprBin String Expr Expr |
    ExprApply Expr [Expr] |
    ExprTuple [Expr] |
    ExprCSID [String] |
    ExprLiteral Literal
    -- TODO: let, dict, array, index
    deriving (Show)

data Literal =
    LitStr String |
    LitInt Integer |
    LitFloat Double
    deriving (Show)
