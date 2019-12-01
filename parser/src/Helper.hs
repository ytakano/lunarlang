module Helper where

import qualified AST

errMsg file (AST.Pos line column) msg =
    "\"" ++ file ++ "\" (line " ++ show line ++ ", column " ++ show column ++ "):\n" ++ msg

msgFilePos file (AST.Pos line column) =
    "\"" ++ file ++ "\" (line " ++ show line ++ ", column " ++ show column ++ ")"

errMsgStack ((file, pos):t) =
    "from " ++ msgFilePos file pos ++ errMsgStack t
