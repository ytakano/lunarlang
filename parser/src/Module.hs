module Module (
    LModule,
    loadFiles
) where

import qualified AST
import           Control.Applicative
import           Control.Exception
import qualified Control.Monad.State as S
import qualified Data.HashMap        as MAP
import qualified Data.HashSet        as SET
import           Debug.Trace
import           Helper
import qualified Parser
import qualified System.Directory    as DIR
import           System.FilePath     ((<.>), (</>))
import qualified System.FilePath     as FP
import qualified System.IO           as IO
import           System.IO.Error
import qualified Text.Pretty.Simple  as PP

data LModule =
    LModule
    FP.FilePath                 -- path to the source
    [FP.FilePath]               -- search path
    [AST.TOP]                   -- AST
    [(AST.Import, FP.FilePath)] -- imported files
    deriving (Show)

data STExtFiles =
    STExtFiles
    LModule            -- Module
    [AST.TOP]          -- AST
    (SET.Set [String]) -- extracted modules
    [(String, AST.Import, [String])] -- result
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
        Right ast -> pure $ LModule file (d:dirs) ast []
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

loadFiles :: [String] -> [String] -> IO (MAP.Map FP.FilePath LModule)
loadFiles files dirs = do
    mod <- loadf files dirs MAP.empty
    fs <- extractFiles (MAP.elems mod)
    loadRecursive mod fs dirs
    where
        loadf [] _ f2m = pure f2m
        loadf (h:t) dirs f2m = do
            h' <- toABS h >>= removeDotDot
            if MAP.member h' f2m then
                loadf t dirs f2m
            else do
                m <- load h' dirs
                loadf t dirs (MAP.insert h' m f2m)

extractFiles [] = pure []
extractFiles (h:t) = do
    let LModule _ path ast _ = h
    let r = S.evalState extractFilesST (STExtFiles h ast SET.empty [])
    PP.pPrint r
    return r

extractFilesST :: S.State STExtFiles [(String, AST.Import, [String])]
extractFilesST = do
    s <- S.get
    let STExtFiles mod ast ex ret = s
    case ast of
        AST.TOPImport im@(AST.Import pos id _):t ->
            if SET.member id ex then do
                let LModule file _ _ _ = mod
                fail $ errMsg file pos "multiply imported"
            else do
                let ex' = SET.insert id ex
                let LModule file path _ _ = mod
                let src = mod2file id ""
                let ret' = (file, im, map (</> src) path) : ret
                S.put $ STExtFiles mod t ex' ret'
                extractFilesST
        h:t -> do
            S.put $ STExtFiles mod t ex ret
            extractFilesST
        _ -> pure $ reverse ret

mod2file [h] file =
    file </> h <.> "lunar"
mod2file (h:t) file =
    mod2file t (file </> h)

loadRecursive dict [] _ = pure dict
loadRecursive _ ((file, AST.Import pos _ _, []):m) _ = do
    let msg = errMsg file pos "could not find module"
    putStrLn msg
    fail "module load error"
loadRecursive dict ((file, ast, h:t):m) dirs =
    if MAP.member h dict then do
        dict' <- appendImportInfo dict file ast h
        loadRecursive dict' m dirs
    else do
        handle <- catch (Just <$> IO.openFile h IO.ReadMode) notFound
        loadHandle handle
    where
        notFound :: IOException -> IO (Maybe IO.Handle)
        notFound e = pure Nothing
        loadHandle Nothing = loadRecursive dict ((file, ast, t):m) dirs
        loadHandle (Just handle) = do
                contents <- IO.hGetContents handle
                case Parser.parse h contents of
                    Right astTop -> do
                        let d = FP.takeDirectory file
                        let mod = LModule h (d:dirs) astTop []
                        let dict1 = MAP.insert h mod dict
                        dict2 <- appendImportInfo dict1 file ast h
                        fs <- extractFiles [mod]
                        loadRecursive dict2 (fs ++ m) dirs
                    Left  err -> print err >> fail "failed to parse"

appendImportInfo dict src ast file =
    case MAP.lookup src dict of
        Just mod -> do
            let LModule s ds a ims = mod
            let mod' = LModule s ds a ((ast, file):ims)
            let dict' = MAP.insert src mod' dict
            pure dict'
        Nothing -> fail "internal error"
