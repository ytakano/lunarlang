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

    -- collect named objects
    let mod1 = MAP.map namedObj mod

    -- resolve identifiers
    mod2 <- resolve mod1

    -- infer kind
    let mod3 = assignKV mod2
        sbst = checkKind mod3
    mod4 <- applySbstDict sbst mod3

    PP.pPrint mod4
