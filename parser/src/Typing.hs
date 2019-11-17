module Typing where

import qualified AST
import qualified Control.Monad.State as S
import qualified Data.HashMap        as MAP
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
