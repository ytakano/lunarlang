module Typing where

import qualified AST
import qualified Control.Monad.State as S
import qualified Data.HashMap        as MAP
import qualified Data.HashSet        as SET
import           Debug.Trace
import           Helper
import           Module
import qualified System.FilePath     as FP

data Named =
    NamedClass AST.ClassDef |
    NamedFunc AST.Defun |
    NamedData AST.Data |
    NamedStruct AST.Struct |
    NamedSum AST.Data AST.SumMem |
    NamedPrim String
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
        updateDict file t id n d pos
            | MAP.member id d = do
                let msg = errMsg file pos "error: multiply used identifier"
                fail msg
            | otherwise = do
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
addSumMem file dt (ast@(AST.SumMem pos id _):t) dict
    | MAP.member id dict = do
        let msg = errMsg file pos "error: multiply used identifier"
        fail msg
    | otherwise = do
        let n = NamedSum dt ast
        let d = MAP.insert id n dict
        addSumMem file dt t d

checkRecMem _ _ [] = pure True
checkRecMem map mod (h:t) = do
    r <- checkUserType map mod h
    if r then
        checkRecMem map mod t
    else
        pure False
    where
        file = mod2file mod

checkUserType map mod (AST.QType _ qual (AST.IDType pos id _)) =
    case findUserObj map mod id of
        Just x  -> case qual of
            Nothing -> checkIDType x
            _       -> pure True
        Nothing -> fail $ errMsg file pos "unkown type specifier"
    where
        file = mod2file mod
        checkIDType (f, mod', obj) = do
            (s, callst) <- S.get
            let callst' = (file, pos):callst
            S.put (s, callst')
            r <- checkRec map mod' (f, obj)
            if r then do
                S.put (s, callst)
                pure True
            else
                pure False
checkUserType _ _ _ = pure True

checkRecSumMem map mod [] = pure True
checkRecSumMem map mod (AST.SumMem _ _ h:t) = do
    r <- checkRecMem map mod h
    if r then
        checkRecSumMem map mod t
    else
        pure False

checkObjRec1st map =
    checkObjRec1st2 map maplist
    where
        maplist = MAP.toList map

checkObjRec1st2 _ [] = True
checkObjRec1st2 map ((file, (mod, obj)):t) =
    checkObjRec1st3 map mod file objlist && checkObjRec1st2 map t
    where
        objlist = MAP.toList obj

checkObjRec1st3 _ _ _ [] = True
checkObjRec1st3 map mod file ((_, obj):t) =
    checkObjRec1st4 map mod file obj && checkObjRec1st3 map mod file t

checkObjRec1st4 map mod file obj =
    S.evalState (checkRec map mod (file, obj)) (SET.empty, [])

checkRec :: File2Mod -> LModule -> (FP.FilePath, Named) -> S.State (SET.Set (FP.FilePath, String), [(FP.FilePath, AST.Position)]) Bool
checkRec map mod (file, NamedData (AST.Data pos (AST.IDPos _ id) _ _ mem)) = do
    (s, callst) <- S.get
    if SET.member (file, id) s then
        fail $ getRecErrMsg callst
    else do
        let s' = SET.insert (file, id) s
        S.put (s', callst)
        r <- checkRecSumMem map mod mem
        if r then do
            S.put (s, callst)
            pure True
        else
            pure False
checkRec _ _ _ = pure True

findUserObj :: File2Mod -> LModule -> [String] -> Maybe (FP.FilePath, LModule, Named)
findUserObj map mod [id]
    | SET.member id primitiveTypes = Just ("", emptyLModule, NamedPrim id)
    | otherwise =
        case getObj objmap of
            Just (mod', x) -> Just (file, mod', x)
            Nothing        -> getObj2 imp
    where
        file = mod2file mod
        imp = mod2imports mod
        objmap = MAP.lookup file map
        getObj (Just (mod', m)) =
            case MAP.lookup id m of
                Just x  -> Just (mod', x)
                Nothing -> Nothing
        getObj Nothing          = Nothing
        getObj2 [] = Nothing
        getObj2 ((AST.Import _ _ AST.ImportHere, imfile):t) =
            case getObj (MAP.lookup imfile map) of
                Just (mod', x) -> Just (imfile, mod', x)
                Nothing        -> getObj2 t
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
        getObj x@(id, obj) ((AST.Import _ id' AST.ImportNS, imfile):t)
            | id == id' = getObj2 obj (MAP.lookup imfile map) imfile
            | otherwise = getObj x t
        getObj x@(id, obj) ((AST.Import _ _ (AST.ImportAs id'), imfile):t)
            | id == id' = getObj2 obj (MAP.lookup imfile map) imfile
            | otherwise = getObj x t
        getObj x (_:t) = getObj x t
        getObj2 obj (Just (mod', objmap)) imfile =
            case MAP.lookup obj objmap of
                Just x  -> Just (imfile, mod', x)
                Nothing -> Nothing
        getObj2 _ _ _ = Nothing

mod2file (LModule f _ _ _) = f
mod2imports (LModule _ _ _ i) = i

getRecErrMsg ((file, pos):t) =
    getRecErrMsg2 t $ errMsg file pos "error: data or struct is recursive"

getRecErrMsg2 [] msg = msg
getRecErrMsg2 ((file, pos):t) msg =
    getRecErrMsg2 t $ msg ++ "\ndefined from: " ++ msgFilePos file pos

primitiveTypes =
    SET.insert "u64" $
    SET.insert "s64" $
    SET.insert "u32" $
    SET.insert "s32" $
    SET.insert "u16" $
    SET.insert "s16" $
    SET.insert "u8" $
    SET.insert "s8" $
    SET.insert "fp64" $
    SET.insert "fp32" $
    SET.insert "void" $
    SET.insert "bool" SET.empty
