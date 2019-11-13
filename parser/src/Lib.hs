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
    let dirs = []
    mod <- MOD.loadFiles [file] dirs MAP.empty
    fs <- MOD.extractFiles (MAP.elems mod)
    mod' <- MOD.loadRecursive mod fs dirs
    PP.pPrint fs
    PP.pPrint mod'
