module Module where

import qualified AST
import qualified Control.Monad.State as S
import qualified Data.HashMap        as MAP
import qualified Data.HashSet        as SET
import           Debug.Trace
import qualified Parser
import qualified System.Directory    as DIR
import           System.FilePath     ((</>))
import qualified System.FilePath     as FP
import qualified System.IO           as IO

data LModule =
    LModule
    FP.FilePath              -- path to the source
    [FP.FilePath]            -- search path
    [AST.TOP]                -- AST
    deriving (Show)

data LModules =
    LModules (MAP.Map FP.FilePath LModule) -- path to module
    [String]                  -- files to be loaded
    LModule                   -- module now loading
    [([String], FP.FilePath)] -- (ID, file) to be loaded
    deriving (Show)

data STExtFiles =
    STExtFiles
    LModule            -- Module
    [AST.TOP]          -- AST
    (SET.Set [String]) -- extracted filenames
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
    let d = FP.takeDirectory file
    handle <- IO.openFile file IO.ReadMode
    contents <- IO.hGetContents handle
    case Parser.parse file contents of
        Right ast -> pure $ LModule file (d:dirs) ast
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

loadFiles [] _ f2m = pure f2m
loadFiles (h:t) dirs f2m = do
    h' <- toABS h >>= removeDotDot
    if MAP.member h' f2m then
        loadFiles t dirs f2m
    else do
        m <- load h dirs
        loadFiles t dirs (MAP.insert h m f2m)

extractFiles (LModule _ dirs ast) =
    dirs

extractFilesFromAST _ [] mods files = files
extractFilesFromAST dirs (AST.Import pos id _:t) mods files =
    if SET.member id mods then files else files
extractFilesFromAST dirs (_:t) mods files =
    extractFilesFromAST dirs t mods files
