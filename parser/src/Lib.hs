module Lib
    ( parseFile
    ) where

import qualified Parser
import qualified System.IO as IO
import qualified Text.Pretty.Simple as PP

parseFile [] = putStrLn "no input file"
parseFile (file:_) = do
    handle <- IO.openFile file IO.ReadMode
    contents <- IO.hGetContents handle
    let result = Parser.parse file contents
    putStrLn file
    putStrLn contents
    putStrLn "AST:"
    PP.pPrint result
