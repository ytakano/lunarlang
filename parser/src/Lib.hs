module Lib
    ( parseFile
    ) where

import qualified Data.HashMap       as MAP
import qualified Module             as MOD
import qualified Parser
import qualified System.IO          as IO
import qualified Text.Pretty.Simple as PP

parseFile [] = putStrLn "no input file"
parseFile files = do
    let dirs = []
    mod <- MOD.loadFiles files dirs
    PP.pPrint mod
