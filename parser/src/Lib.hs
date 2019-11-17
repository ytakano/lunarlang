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
    let mod' = MAP.map namedObj mod
    PP.pPrint mod'
