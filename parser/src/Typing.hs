module Typing where

import qualified AST
import qualified Control.Monad.State as S
import qualified Data.HashMap        as MAP
import qualified Data.HashSet        as SET
import           Helper
import           Module
import qualified System.FilePath     as FP

data Named =
    NamedClass AST.ClassDef |
    NamedFunc AST.Defun |
    NamedData AST.Data |
    NamedStruct AST.Struct |
    NamedSum AST.Data AST.SumMem
    deriving (Show)

data LType = LType LModule (MAP.Map String Named) deriving (Show)

type File2Mod = MAP.Map FP.FilePath (LModule, MAP.Map String Named)

namedObj mod@(LModule file _ ast _) =
    (mod, S.evalState namedObjST (file, ast, MAP.empty))

namedObjST :: S.State (FP.FilePath, [AST.TOP], MAP.Map String Named) (MAP.Map String Named)
namedObjST = do
    (file, ast, dict) <- S.get
    setNamed file ast dict
    where
        updateDict file t id n d pos =
            if MAP.member id d then do
                let msg = errMsg file pos "error: multiply used identifier"
                fail msg
            else do
                let d' = MAP.insert id n d
                S.put (file, t, d')
                namedObjST
        setNamed _ [] d = pure d
        setNamed file (AST.TOPClassDef cls@(AST.ClassDef _ (AST.IDPos pos id) _ _ _) : t) d = do
            let n = NamedClass cls
            updateDict file t id n d pos
        setNamed file (AST.TOPInstance _ : t) d = do
            S.put (file, t, d)
            namedObjST
        setNamed file (AST.TOPDefun f@(AST.Defun _ (AST.IDPos pos id) _ _ _ _) : t) d = do
            let n = NamedFunc f
            updateDict file t id n d pos
        setNamed file (AST.TOPData dt@(AST.Data _ (AST.IDPos pos id) _ _ mem) : t) d = do
            d' <- addSumMem file dt mem d
            let n = NamedData dt
            updateDict file t id n d' pos
        setNamed file (AST.TOPImport _ : t) d = do
            S.put (file, t, d)
            namedObjST
        setNamed file (AST.TOPStruct s@(AST.Struct _ (AST.IDPos pos id) _ _ _) : t) d = do
            let n = NamedStruct s
            updateDict file t id n d pos

addSumMem _ _ [] dict = pure dict
addSumMem file dt (ast@(AST.SumMem pos id _):t) dict =
    if MAP.member id dict then do
        let msg = errMsg file pos "error: multiply used identifier"
        fail msg
    else do
        let n = NamedSum dt ast
        let d = MAP.insert id n dict
        addSumMem file dt t d

--isRecQTypeST :: a -> b -> [AST.QType] -> S.State (SET.Set (FP.FilePath, String)) Bool
checkRecMem map mod (h:t) =
    checkUserType h
    where
        checkUserType (AST.QType _ qual (AST.IDType pos id _)) =
            case findUserObj map mod id of
                Just x  -> case qual of
                    Nothing -> pure True
                    _       -> pure True
                Nothing -> fail $ errMsg file pos "unkown type specifier"
        file = mod2file mod

checkRecSumMem map mod [] = pure True
checkRecSumMem map mod (AST.SumMem _ _ tp:t) = do
    checkRecMem map mod tp
    checkRecSumMem map mod t

checkRec :: File2Mod -> LModule -> (FP.FilePath, Named) -> S.State (SET.Set (FP.FilePath, String), FP.FilePath, AST.Position) Bool
checkRec map mod (file, NamedData (AST.Data _ (AST.IDPos _ id) _ _ mem)) = do
    (s, prevFile, prevPos) <- S.get
    if SET.member (file, id) s then
        fail $ errMsg prevFile prevPos "recursively defined"
    else
        checkRecSumMem map mod mem


findUserObj :: File2Mod -> LModule -> [String] -> Maybe (FP.FilePath, Named)
findUserObj map mod [id] =
    case getObj objmap of
        Just x  -> Just (file, x)
        Nothing -> getObj2 imp
    where
        file = mod2file mod
        imp = mod2imports mod
        objmap = MAP.lookup file map
        getObj (Just (_, m)) = MAP.lookup id m
        getObj Nothing       = Nothing
        getObj2 [] = Nothing
        getObj2 ((AST.Import _ _ AST.ImportHere, imfile):t) =
            case getObj (MAP.lookup imfile map) of
                Just x  -> Just (imfile, x)
                Nothing -> getObj2 t
        getObj2 (_:t) =
            getObj2 t
findUserObj map mod id =
    getObj modid imp
    where
        modid = modid2 $ reverse id
        modid2 (h:t) = (reverse t, h)
        file = mod2file mod
        imp = mod2imports mod
        getObj _ [] = Nothing
        getObj x@(id, obj) ((AST.Import _ id' AST.ImportNS, imfile):t) =
            if id == id' then
                getObj2 obj (MAP.lookup imfile map) imfile
            else
                getObj x t
        getObj x@(id, obj) ((AST.Import _ _ (AST.ImportAs id'), imfile):t) =
            if id == id' then
                getObj2 obj (MAP.lookup imfile map) imfile
            else
                getObj x t
        getObj x (_:t) = getObj x t
        getObj2 obj (Just (_, objmap)) imfile =
            case MAP.lookup obj objmap of
                Just x  -> Just (imfile, x)
                Nothing -> Nothing
        getObj2 _ _ _ = Nothing


mod2file (LModule f _ _ _) = f
mod2imports (LModule _ _ _ i) = i
