module Lib (
    parseFile
) where

import qualified Data.HashMap       as MAP
import           Module
import qualified Parser
import qualified System.IO          as IO
import qualified Text.Pretty.Simple as PP
import           Typing

parseFile [] = putStrLn "no input file"
parseFile files = do
    let dirs = []
    mod <- loadFiles files dirs
    let mod1 = MAP.map namedObj mod
        mod2 = assignKV mod1
        sbst = checkKind mod2
    PP.pPrint mod2
    PP.pPrint sbst
    let isRec = checkObjRec1st mod2
    PP.pPrint isRec
