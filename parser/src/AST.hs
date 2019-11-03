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
    Shared |                -- shared type
    Lin deriving (Show) -- linear type

-- qualified type
data QType = QType Qual LType deriving (Show)

-- type of Lunar language
data LType =
    IDType [String] [QType] |
    TVar String [QType] |
    ArrayType QType |
    TupleType [QType]
    deriving (Show)

data Pred = Pred [String] QType deriving (Show)

data Expr = Expr deriving (Show)
