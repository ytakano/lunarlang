module Lib
    ( parseFile
    ) where

import qualified Data.HashMap       as MAP
import qualified Module             as MOD
import qualified Parser
import qualified System.IO          as IO
import qualified Text.Pretty.Simple as PP

parseFile [] = putStrLn "no input file"
parseFile (file:_) = do
    mod <- MOD.loadFiles [file] [] MAP.empty
    PP.pPrint mod
