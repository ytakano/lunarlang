{-# LANGUAGE FlexibleContexts #-}

module Typing where

import qualified AST
import           Control.Monad       (mapM)
import qualified Control.Monad.State as S
import qualified Data.HashMap        as MAP
import qualified Data.HashSet        as SET
import qualified Data.Maybe          as MB
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
        updateDict file t ident n d pos
            | MAP.member ident d = do
                let msg = errMsg file pos "error: multiply used identifier"
                fail msg
            | otherwise = do
                let d' = MAP.insert ident n d
                S.put (file, t, d')
                namedObjST
        setNamed _ [] d = pure d
        setNamed file (AST.TOPClassDef cls@(AST.ClassDef _ (AST.IDPos pos ident) _ _ _) : t) d = do
            let n = NamedClass cls
            updateDict file t ident n d pos
        setNamed file (AST.TOPInstance _ : t) d = do
            S.put (file, t, d)
            namedObjST
        setNamed file (AST.TOPDefun f@(AST.Defun _ (AST.IDPos pos ident) _ _ _ _) : t) d = do
            let n = NamedFunc f
            updateDict file t ident n d pos
        setNamed file (AST.TOPData dt@(AST.Data _ (AST.IDPos pos ident) _ _ mem) : t) d = do
            d' <- addSumMem file dt mem d
            let n = NamedData dt
            updateDict file t ident n d' pos
        setNamed file (AST.TOPImport _ : t) d = do
            S.put (file, t, d)
            namedObjST
        setNamed file (AST.TOPStruct s@(AST.Struct _ (AST.IDPos pos ident) _ _ _) : t) d = do
            let n = NamedStruct s
            updateDict file t ident n d pos

addSumMem _ _ [] dict = pure dict
addSumMem file dt (ast@(AST.SumMem pos ident _):t) dict
    | MAP.member ident dict = do
        let msg = errMsg file pos "error: multiply used identifier"
        fail msg
    | otherwise = do
        let n = NamedSum dt ast
        let d = MAP.insert ident n dict
        addSumMem file dt t d

checkRecMem dict mod = mapAnd (checkUserType dict mod)

checkUserType dict mod (AST.QType _ qual (AST.IDType pos ident _)) =
    case findUserObj dict mod ident of
        Just x  -> case qual of
            Nothing -> checkIDType x
            _       -> pure True
        Nothing -> fail $ errMsg file pos "unknown type specifier"
    where
        file = mod2file mod
        checkIDType (_, _, NamedFunc _)  = fail $ errMsg file pos "specify type instead of function name"
        checkIDType (_, _, NamedClass _) = fail $ errMsg file pos "specify type instead of class name"
        checkIDType (_, _, NamedSum _ _) = fail $ errMsg file pos "specify type instead of data constructor"
        checkIDType (f, mod', obj) = do
            (s, callst) <- S.get
            let callst' = (file, pos):callst
            S.put (s, callst')
            r <- checkRec dict mod' (f, obj)
            if r then do
                S.put (s, callst)
                pure True
            else
                pure False
checkUserType dict mod (AST.QType _ qual (AST.TupleType _ qts))
    | MB.isNothing qual = mapAnd (checkUserType dict mod) qts
    | otherwise = mapAnd (isDefType dict mod) qts
checkUserType dict mod (AST.QType _ qual (AST.ArrayType _ qt _))
    | MB.isNothing qual = checkUserType dict mod qt
    | otherwise = isDefType dict mod qt
checkUserType _ _ _ = pure True
-- TODO: Function

mapAnd fun t = foldl (&&) True <$> mapM fun t

isDefType dict mod (AST.QType _ _ (AST.IDType pos ident _)) =
    if MB.isJust $ findUserObj dict mod ident then
        pure True
    else
        fail $ errMsg (mod2file mod) pos "unknown type specifier"
isDefType dict mod (AST.QType _ _ (AST.TupleType _ qts))  = mapAnd (isDefType dict mod) qts
isDefType dict mod (AST.QType _ _ (AST.ArrayType _ qt _)) = isDefType dict mod qt
isDefType _ _ _ = pure True
-- TODO: Function

checkRecSumMem dict mod = mapAnd (checkRecMem dict mod . sumMem)
    where
        sumMem (AST.SumMem _ _ m) = m

checkRecProdMem dict mod = mapAnd (checkUserType dict mod . prodMem)
    where
        prodMem (AST.ProdMem _ _ m) = m

checkObjRec1st dict = checkObjRec1st2 dict dictlist
    where
        dictlist = MAP.toList dict

checkObjRec1st2 _ [] = True
checkObjRec1st2 dict ((file, (mod, obj)):t) =
    checkObjRec1st3 dict mod file objlist && checkObjRec1st2 dict t
    where
        objlist = MAP.toList obj

checkObjRec1st3 _ _ _ [] = True
checkObjRec1st3 dict mod file ((_, obj):t) =
    checkObjRec1st4 dict mod file obj && checkObjRec1st3 dict mod file t

checkObjRec1st4 dict mod file obj =
    S.evalState (checkRec dict mod (file, obj)) (SET.empty, [])

checkRec :: File2Mod -> LModule -> (FP.FilePath, Named) -> S.State (SET.Set (FP.FilePath, String), [(FP.FilePath, AST.Position)]) Bool
checkRec dict mod (file, NamedData (AST.Data pos (AST.IDPos _ ident) _ _ mem)) = do
    let fun = checkRecSumMem dict mod mem
    checkRec2 dict mod file ident fun
checkRec dict mod (file, NamedStruct (AST.Struct pos (AST.IDPos _ ident) _ _ mem)) = do
    let fun = checkRecProdMem dict mod mem
    checkRec2 dict mod file ident fun
checkRec _ _ _ = pure True

checkRec2 dict mod file ident fun = do
    state@(s, callst) <- S.get
    if SET.member (file, ident) s then
        fail $ getRecErrMsg callst
    else do
        let s' = SET.insert (file, ident) s
        S.put (s', callst)
        r <- fun
        if r then do
            S.put state
            pure True
        else
            pure False

findUserObj :: File2Mod -> LModule -> [String] -> Maybe (FP.FilePath, LModule, Named)
findUserObj dict mod [ident]
    | SET.member ident primitiveTypes = Just ("", emptyLModule, NamedPrim ident)
    | otherwise =
        case getObj objdict of
            Just (mod', x) -> Just (file, mod', x)
            Nothing        -> getObj2 imp
    where
        file = mod2file mod
        imp = mod2imports mod
        objdict = MAP.lookup file dict
        getObj (Just (mod', m)) =
            case MAP.lookup ident m of
                Just x  -> Just (mod', x)
                Nothing -> Nothing
        getObj Nothing          = Nothing
        getObj2 [] = Nothing
        getObj2 ((AST.Import _ _ AST.ImportHere, imfile):t) =
            case getObj (MAP.lookup imfile dict) of
                Just (mod', x) -> Just (imfile, mod', x)
                Nothing        -> getObj2 t
        getObj2 (_:t) =
            getObj2 t
findUserObj dict mod ident =
    getObj modid imp
    where
        modid = modid2 $ reverse ident
        modid2 (h:t) = (reverse t, h)
        file = mod2file mod
        imp = mod2imports mod
        getObj _ [] = Nothing
        getObj x@(ident, obj) ((AST.Import _ ident' AST.ImportNS, imfile):t)
            | ident == ident' = getObj2 obj (MAP.lookup imfile dict) imfile
            | otherwise = getObj x t
        getObj x@(ident, obj) ((AST.Import _ _ (AST.ImportAs ident'), imfile):t)
            | ident == ident' = getObj2 obj (MAP.lookup imfile dict) imfile
            | otherwise = getObj x t
        getObj x (_:t) = getObj x t
        getObj2 obj (Just (mod', objdict)) imfile =
            case MAP.lookup obj objdict of
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
