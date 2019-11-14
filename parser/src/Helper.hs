module Helper where

import qualified AST

errMsg file (AST.Pos line column) msg =
    "\"" ++ file ++ "\" (line " ++ show line ++ ", column " ++ show column ++ "):\n" ++ msg
