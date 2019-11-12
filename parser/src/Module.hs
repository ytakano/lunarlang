module Module where

import qualified AST
--import qualified Control.Monad.State as S
import qualified Data.HashMap     as M
import           Debug.Trace
import qualified Parser
import qualified System.Directory as DIR
import           System.FilePath  ((</>))
import qualified System.FilePath  as FP
import qualified System.IO        as IO

data LModule =
    LModule
    FP.FilePath              -- path to the source
    [FP.FilePath]            -- search path
    [AST.TOP]                -- AST
    (M.Map [String] LModule) -- identifier to module
    deriving (Show)

data LModules =
    LModules (M.Map FP.FilePath LModule) -- path to module
    [String]                  -- files to be loaded
    LModule                   -- module now loading
    [([String], FP.FilePath)] -- (ID, file) to be loaded
    deriving (Show)

{-
    input:  relative of absolute path
    output: absolute path
-}
toABS file = do
    dir <- DIR.getCurrentDirectory
    pure $ if FP.isRelative file then
        FP.normalise (dir </> file)
    else
        FP.normalise file

{-
    input:
        file: finename
        dirs: search paths
    output:
        IO LModule
-}
load file dirs = do
    file' <- toABS file >>= removeDotDot
    let d = FP.takeDirectory file'
    handle <- IO.openFile file IO.ReadMode
    contents <- IO.hGetContents handle
    case Parser.parse file contents of
        Right ast -> pure $ LModule file' (d:dirs) ast M.empty
        Left  err -> print err >> fail "failed to parse"

{-
    input:  "/a/b/../c"
    output: "/a/c"
-}
removeDotDot file = do
    let f = rmd (reverse (FP.splitDirectories file)) []
    case f of
        Just f' -> pure f'
        Nothing -> fail ("invalid file name \"" ++ file ++ "\"")
    where
        rmd [] ret         = Just $ FP.joinPath ret
        rmd ["..", _] _    = Nothing
        rmd ("..":h:t) ret = rmd t ret
        rmd (h:t) ret      = rmd t (h:ret)
