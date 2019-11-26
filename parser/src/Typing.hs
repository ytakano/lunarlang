{-# LANGUAGE FlexibleContexts #-}

module Typing where

import qualified AST
import           Control.Monad       (mapM)
import qualified Control.Monad.State as S
import qualified Data.HashMap        as MAP
import qualified Data.HashSet        as SET
import qualified Data.List           as L
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
    NamedSum String AST.SumMem |
    NamedPrim String
    deriving (Show)

data LType = LType LModule (MAP.Map String Named) deriving (Show)

type File2Mod = MAP.Map FP.FilePath (LModule, MAP.Map String Named)

data Kind = Star | Kfun Kind Kind deriving (Show, Eq)

data Type =
    TVar Tyvar |
    TCon Tycon |
    TAp Type Type |
    TGen Int
    deriving (Show, Eq)

data Tyvar = Tyvar Id Kind deriving (Show, Eq)
data Tycon = Tycon (Maybe FP.FilePath) Id Kind deriving (Show, Eq)

type Id = String

class HasKind t where
    kind :: t -> Kind
instance HasKind Tyvar where
    kind (Tyvar _ k) = k
instance HasKind Tycon where
    kind (Tycon _ _ k) = k
instance HasKind Type where
    kind (TCon tc) = kind tc
    kind (TVar u)  = kind u
    kind (TAp t _) = case kind t of (Kfun _ k) -> k

type Subst = [(Tyvar, Type)]

class Types t where
    apply :: Subst -> t -> t
    tv    :: t -> [Tyvar]

instance Types Type where
    apply s (TVar u)  = MB.fromMaybe (TVar u) (lookup u s)
    apply s (TAp l r) = TAp (apply s l) (apply s r)
    apply s t         = t

    tv (TVar u)  = [u]
    tv (TAp l r) = tv l `L.union` tv r
    tv _         = []

instance Types a => Types [a] where
    apply s = map (apply s)
    tv = L.nub . concatMap tv

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ reverse (foldl chk [] s1)
    where
        chk ret a
            | any (hasKey a) s2 = []
            | otherwise         = a:ret
        hasKey (k1, _) (k2, _) = k1 == k2

nullSubst = []

(+->) :: Tyvar -> Type -> Subst
u +-> t = [(u, t)]

{-
    get user defined types from module
-}
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
            d' <- addSumMem file (dataId dt) mem d
            let n = NamedData dt
            updateDict file t ident n d' pos
            where
                dataId (AST.Data _ (AST.IDPos _ ident) _ _ _)= ident
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
            d = MAP.insert ident n dict
        addSumMem file dt t d

-- checkRecMem dict mod = mapAnd (checkUserType dict mod)

-- checkUserType dict mod (AST.QType _ qual (AST.IDType pos ident typeArgs)) = do
--     r <- mapAnd (isDefType dict mod) typeArgs
--     if r then
--         case findUserObj dict mod ident of
--             Just x  -> checkIDType qual x
--             Nothing -> fail $ errMsg file pos "unknown type specifier"
--     else
--         pure False
--     where
--         file = mod2file mod
--         msg  = errMsg file pos
--         lenTargs = length typeArgs
--         checkIDType _ (_, _, NamedFunc _)  = fail $ msg "specify type instead of function name"
--         checkIDType _ (_, _, NamedClass _) = fail $ msg "specify type instead of class name"
--         checkIDType _ (_, _, NamedSum _ _) = fail $ msg "specify type instead of data constructor"
--         checkIDType Nothing (f, mod', obj) = do
--             r1 <- checkTArgs file pos lenTargs obj
--             (s, callst) <- S.get
--             let callst' = (file, pos):callst
--             S.put (s, callst')
--             r2 <- checkRec dict mod' (f, obj)
--             -- TODO: apply type arguments to mod'
--             if r1 && r2 then do
--                 S.put (s, callst)
--                 pure True
--             else
--                 pure False
--         checkIDType _ (_, _, obj) = checkTArgs file pos lenTargs obj
-- checkUserType dict mod (AST.QType _ qual (AST.TupleType _ qts))
--     | MB.isNothing qual = mapAnd (checkUserType dict mod) qts
--     | otherwise = mapAnd (isDefType dict mod) qts
-- checkUserType dict mod (AST.QType _ qual (AST.ArrayType _ qt _))
--     | MB.isNothing qual = checkUserType dict mod qt
--     | otherwise = isDefType dict mod qt
-- checkUserType dict mod func@(AST.QType _ _ AST.FuncType{}) = isDefType dict mod func
-- checkUserType _ _ _ = pure True

-- mapAnd fun t = foldl (&&) True <$> mapM fun t

-- checkTArgs file pos lenTargs (NamedData (AST.Data _ (AST.IDPos _ ident) tv _ _))     = checkLen file pos lenTargs ident tv
-- checkTArgs file pos lenTargs (NamedStruct (AST.Struct _ (AST.IDPos _ ident) tv _ _)) = checkLen file pos lenTargs ident tv
-- checkTArgs file pos lenTargs _ =
--     if lenTargs == 0 then
--         pure True
--     else
--         fail $ errMsg file pos "cannot pass type arguments"

-- checkLen file pos lenTargs ident tv =
--     if length tv == lenTargs then
--         pure True
--     else
--         fail $ errMsg file pos (ident ++ " requires " ++ show (length tv)
--             ++ " type arguments, but " ++ show lenTargs
--             ++ " type arguments are passed")

-- isDefType dict mod (AST.QType _ _ (AST.IDType pos ident typeArgs)) = do
--     r <- mapAnd (isDefType dict mod) typeArgs
--     if r then
--         case findUserObj dict mod ident of
--             Just (_, _, NamedFunc _)  -> fail $ msg "specify type instead of function name"
--             Just (_, _, NamedClass _) -> fail $ msg "specify type instead of class name"
--             Just (_, _, NamedSum _ _) -> fail $ msg "specify type instead of data constructor"
--             Just (_, _, obj)          -> checkTArgs file pos lenTargs obj
--             Nothing                   -> fail $ msg "unknown type specifier"
--     else
--         pure False
--     where
--         file = mod2file mod
--         msg  = errMsg file pos
--         lenTargs = length typeArgs
-- isDefType dict mod (AST.QType _ _ (AST.TupleType _ qts))  = mapAnd (isDefType dict mod) qts
-- isDefType dict mod (AST.QType _ _ (AST.ArrayType _ qt _)) = isDefType dict mod qt
-- isDefType dict mod (AST.QType _ _ (AST.FuncType _ args ret)) = do
--     a <- mapAnd (isDefType dict mod) args
--     b <- isDefType dict mod ret
--     pure $ a && b
-- isDefType _ _ _ = pure True

-- checkRecSumMem dict mod = mapAnd (checkRecMem dict mod . sumMem)
--     where
--         sumMem (AST.SumMem _ _ m) = m

-- checkRecProdMem dict mod = mapAnd (checkUserType dict mod . prodMem)
--     where
--         prodMem (AST.ProdMem _ _ m) = m

-- checkObjRec1st dict = checkObjRec1st2 dict dictlist
--     where
--         dictlist = MAP.toList dict

-- checkObjRec1st2 _ [] = True
-- checkObjRec1st2 dict ((file, (mod, obj)):t) =
--     checkObjRec1st3 dict mod file objlist && checkObjRec1st2 dict t
--     where
--         objlist = MAP.toList obj

-- checkObjRec1st3 _ _ _ [] = True
-- checkObjRec1st3 dict mod file ((_, obj):t) =
--     checkObjRec1st4 dict mod file obj && checkObjRec1st3 dict mod file t

-- checkObjRec1st4 dict mod file obj =
--     S.evalState (checkRec dict mod (file, obj)) (SET.empty, [])

-- checkRec :: File2Mod -> LModule -> (Maybe FP.FilePath, Named) -> S.State (SET.Set (FP.FilePath, String), [(FP.FilePath, AST.Position)]) Bool
-- checkRec dict mod (Just file, NamedData (AST.Data pos (AST.IDPos _ ident) _ _ mem)) = do
--     let fun = checkRecSumMem dict mod mem
--     checkRec2 dict mod file ident fun
-- checkRec dict mod (Just file, NamedStruct (AST.Struct pos (AST.IDPos _ ident) _ _ mem)) = do
--     let fun = checkRecProdMem dict mod mem
--     checkRec2 dict mod file ident fun
-- checkRec _ _ _ = pure True

-- checkRec2 dict mod file ident fun = do
--     state@(s, callst) <- S.get
--     if SET.member (file, ident) s then
--         fail $ getRecErrMsg callst
--     else do
--         let s' = SET.insert (file, ident) s
--         S.put (s', callst)
--         r <- fun
--         if r then do
--             S.put state
--             pure True
--         else
--             pure False

findUserObj :: File2Mod -> LModule -> [String] -> Maybe (Maybe FP.FilePath, LModule, Named)
findUserObj dict mod [ident]
    | SET.member ident primitiveTypes = Just (Nothing, emptyLModule, NamedPrim ident)
    | otherwise =
        case getObj objdict of
            Just (mod', x) -> Just (Just file, mod', x)
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
                Just (mod', x) -> Just (Just imfile, mod', x)
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
                Just x  -> Just (Just imfile, mod', x)
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

{-
    assing kind variable
-}
assignKV dict = S.evalState assignKV' (0, dict)

assignKV' :: S.State (Int, File2Mod) File2Mod
assignKV' = do
    (_, dict) <- S.get
    assignKV2 (MAP.elems dict)

assignKV2 [] = do
    (_, f2m) <- S.get
    pure f2m
assignKV2 ((mod, namedDict):t) = do
    assignKV3 mod (MAP.toList namedDict)
    assignKV2 t

assignKV3 mod [] = pure ()
assignKV3 mod ((ident, NamedData (AST.Data pos iddata tv preds mem)):t) = do
    (i, f2m) <- S.get
    let (tv', i') = assignKV4 tv i []
        named = NamedData (AST.Data pos iddata tv' preds mem)
        f2m' = insertNamed (mod2file mod) ident named f2m
    S.put (i', f2m')
    assignKV3 mod t
assignKV3 mod ((ident, NamedStruct (AST.Struct pos iddata tv preds mem)):t) = do
    (i, f2m) <- S.get
    let (tv', i') = assignKV4 tv i []
        named = NamedStruct (AST.Struct pos iddata tv' preds mem)
        f2m' = insertNamed (mod2file mod) ident named f2m
    S.put (i', f2m')
    assignKV3 mod t
assignKV3 mod (h:t) = do
    assignKV3 mod t

assignKV4 [] i ret = (reverse ret, i)
assignKV4 (AST.TypeVarKind pos ident Nothing:t) i ret =
    assignKV4 t (i + 1) (AST.TypeVarKind pos ident (Just $ AST.KV i):ret)
assignKV4 (h:t) i ret =
    assignKV4 t i (h:ret)

insertNamed file ident named dict =
    case MAP.lookup file dict of
        Just (mod, dict') ->
            let d1 = MAP.insert ident named dict' in
                MAP.insert file (mod, d1) dict
        Nothing -> dict

{-
    take kind constraint and return kind substitution
-}
unifyKind :: [(AST.Kind, AST.Kind)] -> Maybe [(AST.Kind, AST.Kind)]
unifyKind [] = Just []
unifyKind ((AST.KStar, AST.KStar):t) = unifyKind t
unifyKind (k@(a@(AST.KV _), v):t) =
    if hasFVKind a v then
        Nothing
    else
        case unifyKind t' of
            Just s  -> Just $ composeSbstKind s [k]
            Nothing -> Nothing
    where
        t' = map app t
        app (t1, t2) = (applySbstKind k t1, applySbstKind k t2)
unifyKind ((v, k@(AST.KV _)):t) =
    unifyKind ((k, v):t)
unifyKind ((AST.KArray t1 t2, AST.KArray t1' t2'):t) =
    unifyKind ((t1, t1'):(t2, t2'):t)
unifyKind _ =
    Nothing

applySbstKind _ AST.KStar  = AST.KStar
applySbstKind (AST.KV var1, v) k@(AST.KV var2)
    | var1 == var2 = v
    | otherwise = k
applySbstKind sbst (AST.KArray lhs rhs) =
    AST.KArray lhs' rhs'
    where
        lhs' = applySbstKind sbst lhs
        rhs' = applySbstKind sbst rhs

applySbstKindArr t term = foldl (flip applySbstKind) term t

composeSbstKind :: [(AST.Kind, AST.Kind)] -> [(AST.Kind, AST.Kind)] -> [(AST.Kind, AST.Kind)]
composeSbstKind s1 s2 =
    [(k, applySbstKindArr s1 v) | (k, v) <- s2] ++ diffSbstKind s1 s2 []

diffSbstKind [] _ ret = reverse ret
diffSbstKind (h@(k@(AST.KV var1), _):t) s2 ret =
    if any hasKey s2 then
        diffSbstKind t s2 ret
    else
        diffSbstKind t s2 (h:ret)
    where
        hasKey (k', v) = k == k'

hasFVKind _ AST.KStar                = False
hasFVKind (AST.KV var) (AST.KV var') = var == var'
hasFVKind v (AST.KArray t1 t2)       = hasFVKind v t1 || hasFVKind v t2
hasFVKind _ _                        = False

{-
    s: substitution
    a, b, c: type
    `a: type variable
    qual: uniq | shared

    if qual? a then
        ak = a's kind
        unify(ak, *)
        return *
    if qual? `a then
        ak = a's kind
        s' = unify(ak, *)
        s = s' s
        return *
    else checkKindIn
-}
checkKindTop mod (AST.QType _ _ (AST.IDType pos ident [])) = do
    (sbst, dict, _) <- S.get
    case findUserObj dict mod ident of
        Nothing -> fail $ err "unknown type specifier"
        Just (_, _, n) -> do
            k <- getKind mod pos n
            case unifyKind [(k, AST.KStar)] of
                Nothing -> fail $ err "kind must be *"
                _       -> pure AST.KStar
    where
        err = errMsg (mod2file mod) pos
checkKindTop mod (AST.QType _ _ (AST.TVar pos ident [])) = do
    (sbst, dict, tv2kind) <- S.get
    case MAP.lookup ident tv2kind of
        Nothing -> fail $ err "undefined type variable"
        Just k  ->
            case unifyKind [(k, AST.KStar)] of
                Nothing -> fail $ err "kind must be *"
                Just s  -> do
                    let s' = composeSbstKind s sbst
                    S.put (s', dict, tv2kind)
                    pure AST.KStar
    where
        err = errMsg (mod2file mod) pos
checkKindTop mod qt = checkKindIn mod qt

{-
    s: substitution
    a, b, c: type
    `a: type variable
    qual: uniq | shared

    if a then return a's kind
    if qual a then
        ak = a's kind
        unify(ak, *)
        return *
    if qual? a<b,c> then
        ak = a's kind
        bk = b's kind
        ck = c's kind
        s' = unify(ak, bk -> ck -> *)
        s = s' s
        return *
    if `a then return `a's kind
    if qual `a then
        ak = `a's kind
        s' = unify(ak, *)
        s = s' s
        return *
    if qual? `a<b,c> then
        ak = `a's kind
        bk = b's kind
        ck = c's kind
        s' = unify(ak, bk -> ck -> *)
        s = s' s
        return *
    if qual? [a] then checkKindTop a
    if qual? (a, b) then
        checkKindTop a
        checkKindTop b
        return *
    if qual? func (a, b) -> c then
        checkKindTop a
        checkKindTop b
        checkKindTop c
        return *
    if qual? void then return *
-}
checkKindIn mod (AST.QType _ qual (AST.IDType pos ident qt)) = do
    (sbst, dict, _) <- S.get
    case findUserObj dict mod ident of
        Nothing        -> fail $ err "unknown type specifier"
        Just (_, _, n) -> do
            k <- getKind mod pos n
            case qt of
                [] -> checkQual (applySbstKindArr sbst k) qual
                _  -> checkKindUnify mod pos k qt
    where
        err = errMsg (mod2file mod) pos
        checkQual k Nothing = pure k
        checkQual k _ =
            case unifyKind [(k, AST.KStar)] of
                Nothing -> fail $ err "kind must be *"
                Just s  -> pure AST.KStar
checkKindIn mod (AST.QType _ qual (AST.TVar pos ident qt)) = do
    (sbst, _, tv2kind) <- S.get
    case MAP.lookup ident tv2kind of
        Nothing -> fail $ err "undefined type variable"
        Just k  -> case qt of
            [] -> checkQual (applySbstKindArr sbst k) qual
            _  -> checkKindUnify mod pos k qt
    where
        err = errMsg (mod2file mod) pos
        checkQual k Nothing = pure k
        checkQual k _ =
            case unifyKind [(k, AST.KStar)] of
                Nothing -> fail $ err "kind must be *"
                Just s  -> do
                    (sbst, dict, tv2kind) <- S.get
                    let s' = composeSbstKind s sbst
                    S.put (s', dict, tv2kind)
                    pure AST.KStar
checkKindIn mod (AST.QType _ _ (AST.ArrayType _ qt _)) = checkKindTop mod qt
checkKindIn mod (AST.QType _ _ (AST.TupleType _ qt)) = do
    mapM_ (checkKindTop mod) qt
    pure AST.KStar
checkKindIn mod (AST.QType _ _ (AST.FuncType _ args ret)) = do
    mapM_ (checkKindTop mod) args
    checkKindTop mod ret
    pure AST.KStar
checkKindIn _ _ = pure AST.KStar

checkKindUnify mod pos k qt = do
    k2 <- foldr AST.KArray AST.KStar <$> mapM (checkKindIn mod) qt
    (sbst, dict, tv2kind) <- S.get
    let k1 = applySbstKindArr sbst k
    case unifyKind [(k1, k2)] of
        Nothing -> fail $ err "kind mismatch"
        Just s -> do
            let s' = composeSbstKind s sbst
            S.put (s', dict, tv2kind)
            pure AST.KStar
    where
        err = errMsg (mod2file mod) pos

getKind _ _ (NamedStruct (AST.Struct _ _ tv _ _)) = getKindTVK tv
getKind _ _ (NamedData (AST.Data _ _ tv _ _))     = getKindTVK tv
getKind _ _ (NamedPrim _)                         = pure AST.KStar
getKind mod pos _     = fail $ errMsg (mod2file mod) pos "must be data, struct, or primitive type"

getKindTVK tv = foldr AST.KArray AST.KStar <$> karr
    where
        ks (AST.TypeVarKind _ _ (Just k)) = pure k
        ks _                              = fail "internal error"
        karr = mapM ks tv

checkKindDataMem mod (AST.Data _ ident tv _ mem) = do
    updateTv tv
    mapM_ (checkKindMem mod . getMem) mem
    where
        getMem (AST.SumMem _ _ m) = m

checkKindStructMem mod (AST.Struct _ _ tv _ mem) = do
    updateTv tv
    mapM_ (checkKindTop mod . getMem) mem
    where
        getMem (AST.ProdMem _ _ m) = m

updateTv tv = do
    tv' <- getMapTV2Kind tv
    (sbst, dict, _) <- S.get
    S.put (sbst, dict, tv')

checkKindMem mod = mapM_ (checkKindTop mod)

checkKindNamed mod (NamedData d@AST.Data{})     = checkKindDataMem mod d
checkKindNamed mod (NamedStruct d@AST.Struct{}) = checkKindStructMem mod d
checkKindNamed _ _                              = pure ()

getMapTV2Kind tv = do
    v <- mapM val tv
    let k = map key tv
        dict = MAP.fromList $ zip k v
    pure dict
    where
        key (AST.TypeVarKind _ k _) = k
        val (AST.TypeVarKind _ _ (Just v)) = pure v
        val _                              = fail "internal error"

checkKind dict =
    S.evalState fun ([], dict, MAP.empty)
    where
        elems = MAP.elems dict
        fun = do
            mapM_ checkKindModDict elems
            (sbst, _, _) <- S.get
            pure sbst

checkKindModDict (mod, dict) =
    mapM_ (checkKindNamed mod) elems
    where
        elems = MAP.elems dict

applySbstTVK file sbst (AST.TypeVarKind pos ident (Just k)) =  do
    let k' = applySbstKindArr sbst k
    case k' of
        AST.KV _ -> fail $ errMsg file pos "could not infer kind. specify kind statically"
        _ ->  pure $ AST.TypeVarKind pos ident (Just k')

applySbstNamed file sbst (NamedData (AST.Data pos ident tv pred mem)) = do
    tv' <- mapM (applySbstTVK file sbst) tv
    pure $ NamedData (AST.Data pos ident tv' pred mem)
applySbstNamed file sbst (NamedStruct (AST.Struct pos ident tv pred mem)) = do
    tv' <- mapM (applySbstTVK file sbst) tv
    pure $ NamedStruct (AST.Struct pos ident tv' pred mem)
applySbstNamed _ _ x = pure x

applySbstDict sbst = mapM app
    where
        app (mod, dict) = do
            dict' <- mapM (applySbstNamed (mod2file mod) sbst) dict
            pure (mod, dict')
