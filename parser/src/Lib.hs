module Lib
    ( parseFile
    ) where

import qualified Module             as M
import qualified Parser
import qualified System.IO          as IO
import qualified Text.Pretty.Simple as PP

parseFile [] = putStrLn "no input file"
parseFile (file:_) = do
    mod <- M.load file []
    PP.pPrint mod
