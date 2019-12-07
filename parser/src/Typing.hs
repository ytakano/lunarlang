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
data Tycon = Tycon PathID Kind deriving (Show, Eq)

type Id = String
data PathID = PathID (Maybe FP.FilePath) Id deriving (Show, Eq)

data STRec = STRec {
    visitedType :: [PathID],
    checkedType :: [Type],
    posStack    :: [(FP.FilePath, AST.Position)],
    tvSeq       :: Int
} deriving (Show)


class HasKind t where
    kind :: t -> Kind
instance HasKind Tyvar where
    kind (Tyvar _ k) = k
instance HasKind Tycon where
    kind (Tycon _ k) = k
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

nullSubst = []

(+->) :: Tyvar -> Type -> Subst
u +-> t = [(u, t)]

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ reverse (foldl chk [] s1)
    where
        chk ret a
            | any (hasKey a) s2 = []
            | otherwise         = a:ret
        hasKey (k1, _) (k2, _) = k1 == k2

mergeM :: Monad m => Subst -> Subst -> m Subst
mergeM s1 s2 =
    case merge s1 s2 of
        Just s  -> pure s
        Nothing -> fail "merge fails"

merge :: Subst -> Subst -> Maybe Subst
merge s1 s2 = if agree then Just (s1 ++ s2) else Nothing
    where
        agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) prod
        prod  = map fst s1 `L.intersect` map fst s2

mgu :: Monad m => Type -> Type -> m Subst
mgu (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    pure $ s2 @@ s1
mgu (TVar u) t  = varBind u t
mgu t (TVar u)  = varBind u t
mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2 = pure nullSubst
mgu _ _          = fail "types do not unify"

varBind :: Monad m => Tyvar -> Type -> m Subst
varBind u t
    | t == TVar u      = pure nullSubst
    | u `elem` tv t    = fail "occurs check fails"
    | kind u /= kind t = fail "kinds do not match"
    | otherwise        = pure $ u +-> t

{-
    s: substitution
    t1, t2: type

    if ∃s (s t1 = t2) then s else fail
-}
matchM lhs rhs =
    case match lhs rhs of
        Just s  -> pure s
        Nothing -> fail "types do not match"

match (TAp l r) (TAp l' r') =
    case (match l l', match r r') of
        (Just sl, Just sr) -> merge sl sr
        _                  -> Nothing
match (TVar u) t
    | kind u == kind t = Just $ u +-> t
match (TCon tc1) (TCon tc2)
    | tc1 == tc2       = Just nullSubst
match _ _              = Nothing

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

{-
    if a <b, c> then
        if a ∈ visited then fail

        push a to posStack

        bt = b's type (checkRecIn b)
        ct = c's type (checkRecIn c)

        checkRecNamedIn a <bt, ct>

        pop from posStack
        return a <bt, ct>
    if qual a <b, c> then
        checkRecIn qual a <b, c>
-}
checkRecTop :: LModule -> File2Mod -> AST.QType -> S.State STRec Type
checkRecTop mod f2m (AST.QType _ Nothing (AST.IDType ident@AST.IDAbs{} qts)) = do
    s <- S.get
    let ident' = AST.idAbsID ident
        fp  = AST.idAbsFile ident
        pos = AST.idAbsPos ident
        pid = PathID (Just $ mod2file mod) ident'
        vt  = visitedType s
        ps  = posStack s

    if pid `elem` vt then do
        -- check visited
        let msg  = errMsg (mod2file mod) pos "data or struct is recursive"
            msgs = errMsgStack ps
        fail $ msg ++ msgs
    else
        case findObjFromABSPath f2m fp ident' of
            Just (mod', nm) -> do
                -- push to posStack
                S.put $ s { posStack = (mod2file mod, pos):ps }

                args <- mapM (checkRecIn mod f2m) qts
                ret  <- named2Type mod' args nm

                -- apply qts to nm
                nm' <- applyQTypes2Named mod qts nm
                checkRecNamedIn mod' f2m nm'

                -- pop from posStack
                s2 <- S.get
                S.put $ s2 { posStack = ps }

                pure ret
                -- TODO: Nothing

{-
    if qual? a <b, c> then
        v = visited
        visited = []
        push a to posStack

        bt = b's type (checkRecIn b)
        ct = c's type (checkRecIn c)

        at = checkRecNamed a <bt, ct>

        pop from posStack
        visited = v
        return qual at
-}
checkRecIn :: LModule -> File2Mod -> AST.QType -> S.State STRec Type
checkRecIn mod f2m (AST.QType _ qual (AST.IDType ident qts)) = do
    s <- S.get
    let ident' = AST.idAbsID ident
        fp  = AST.idAbsFile ident
        pos = AST.idAbsPos ident
        vt  = visitedType s
        ps  = posStack s
    S.put $ s { visitedType = [], posStack = (mod2file mod, pos):ps }

    ts <- mapM (checkRecIn mod f2m) qts

    case findObjFromABSPath f2m fp ident' of
        Just (mod', nm) -> do
            ret <- checkRecNamed mod' f2m qts ts nm
            -- pop from posStack and restore update visited
            s2 <- S.get
            S.put $ s2 { visitedType = vt, posStack = ps }

            pure $ getQualType qual ret

{-
    s: substitution

    if data foo <a, b> then
        if ∃c ∃s (ck ∈ checked ∧ apply(s, foo <a, b>) = c) then
            return foo <a, b>
        else
            add foo <a, b> to checked
            checkRecNamedIn foo <a, b>
            return foo <a, b>
-}
checkRecNamed :: LModule -> File2Mod -> [AST.QType] -> [Type] -> Named -> S.State STRec Type
checkRecNamed mod f2m argsAST argsType (NamedData d@(AST.Data pos ident tvk _ mem)) = do
    s <- S.get
    t <- data2Type mod argsType d
    let ck = checkedType s
        isChecked = any MB.isJust $ map (match t) ck
    if isChecked then
        pure t
    else do
        -- update state
        let ps = posStack s
            s' = s { checkedType = t : ck,
                     posStack = (mod2file mod, pos):ps }
        S.put s'

        tv2qt <- getTV2QType tvk argsAST
        mem'  <- mapM (applyTv2QTypeSumMem mod tv2qt) mem

        checkRecNamedIn mod f2m (NamedData (d { AST.dataMem = mem' }))
        pure t

checkRecNamedIn mod f2m (NamedData (AST.Data _ (AST.IDPos _ ident) _ pred mem)) = do
    -- push to visited
    s <- S.get
    let vt = visitedType s
        s' = s { visitedType = PathID (Just $ mod2file mod) ident : vt }
    S.put s'

    mapM_ (checkRecSumMem mod f2m) mem

    -- pop from visited
    s2 <- S.get
    S.put $ s2 { visitedType = vt }

checkRecSumMem mod f2m (AST.SumMem _ _ mem) = mapM_ (checkRecTop mod f2m) mem

applyQTypes2Named mod args (NamedData d@AST.Data{}) = do
    let tvk = AST.dataTVK d
        mem = AST.dataMem d
    tv2qt <- getTV2QType tvk args
    mem' <- mapM (applyTv2QTypeSumMem mod tv2qt) mem
    pure $ NamedData (d { AST.dataMem = mem' })

applyTv2QTypeSumMem mod tv2qt (AST.SumMem pos ident mem) = do
    mem' <- mapM (applyTv2QType mod tv2qt) mem
    pure $ AST.SumMem pos ident mem'

applyTv2QType mod tv2qt qt@(AST.QType pos (Just qual) (AST.TVar _ ident _)) =
    case MAP.lookup ident tv2qt of
        Just arg@(AST.QType _ (Just qual') _) -> do
            -- error: applying qual a to `b of qual `b
            s <- S.get
            let msg  = errMsg (mod2file mod) pos "qualifiers are nested"
                msgs = errMsgStack $ posStack s
            fail $ msg ++ msgs
        Just arg -> applyTv2QType2 mod tv2qt qt arg
        Nothing  -> pure qt
applyTv2QType mod tv2qt qt@(AST.QType pos Nothing tv@(AST.TVar _ ident _)) =
    case MAP.lookup ident tv2qt of
        Just arg@(AST.QType _ argQt _) -> applyTv2QType2 mod tv2qt (AST.QType pos argQt tv) arg
        Nothing                        -> pure qt
applyTv2QType mod tv2qt (AST.QType pos1 qual (AST.IDType ident qts)) = do
    qts' <- mapM (applyTv2QType mod tv2qt) qts
    pure $ AST.QType pos1 qual (AST.IDType ident qts')
applyTv2QType mod tv2qt (AST.QType pos1 qual (AST.ArrayType pos2 qt ex)) = do
    qt' <- applyTv2QType mod tv2qt qt
    pure $ AST.QType pos1 qual (AST.ArrayType pos2 qt' ex)
applyTv2QType mod tv2qt (AST.QType pos1 qual (AST.TupleType pos2 qts)) = do
    qts' <- mapM (applyTv2QType mod tv2qt) qts
    pure $ AST.QType pos1 qual (AST.TupleType pos2 qts')
applyTv2QType mod tv2qt (AST.QType pos1 qual (AST.FuncType pos2 args ret)) = do
    args' <- mapM (applyTv2QType mod tv2qt) args
    ret'  <- applyTv2QType mod tv2qt ret
    pure $ AST.QType pos1 qual (AST.FuncType pos2 args' ret')
applyTv2QType _ _ qt = pure qt

setPos2ID pos ident@AST.IDRel{} = ident { AST.idRelPos = pos }
setPos2ID pos ident@AST.IDAbs{} = ident { AST.idAbsPos = pos }

{-
    apply a type to a type variable
-}
applyTv2QType2 mod tv2qt qt@AST.QType{ AST.qtypeType = AST.TVar pos _ targs} AST.QType{ AST.qtypeType = AST.IDType ident []} = do
    -- ex: apply a to `b of `b<c, d>
    targs' <- mapM (applyTv2QType mod tv2qt) targs
    let ident' = setPos2ID pos ident
        t = AST.IDType ident' targs'
    pure $ qt { AST.qtypeType = t }
applyTv2QType2 mod _ qt@AST.QType{ AST.qtypeType = AST.TVar pos _ []} AST.QType{ AST.qtypeType = AST.IDType ident targs} = do
    -- ex:: apply a<b, c> to `d
    let ident' = setPos2ID pos ident
        t = AST.IDType ident' targs
    pure $ qt { AST.qtypeType = t }
applyTv2QType2 mod tv2qt qt@AST.QType{ AST.qtypeType = AST.TVar pos _ targs} AST.QType{ AST.qtypeType = AST.TVar _ ident []} = do
    -- ex: apply `a to `b of `b<c, d>
    targs' <- mapM (applyTv2QType mod tv2qt) targs
    let t = AST.TVar pos ident targs'
    pure $ qt { AST.qtypeType = t }
applyTv2QType2 mod _ qt@AST.QType{ AST.qtypeType = AST.TVar pos _ []} AST.QType{ AST.qtypeType = AST.TVar _ ident targs} = do
    -- ex: apply `a<b, c> to `d
    let t = AST.TVar pos ident targs
    pure $ qt { AST.qtypeType = t }
applyTv2QType2 mod _ qt@AST.QType{ AST.qtypeType = AST.TVar pos _ []} AST.QType{ AST.qtypeType = AST.ArrayType _ arrqt arrexpr} = do
    -- ex: apply [a] to `b
    let t = AST.ArrayType pos arrqt arrexpr
    pure $ qt { AST.qtypeType = t }
applyTv2QType2 mod _ qt@AST.QType{ AST.qtypeType = AST.TVar pos _ []} AST.QType{ AST.qtypeType = AST.TupleType _ tupqt} = do
    -- ex: apply (a, b) to `c
    let t = AST.TupleType pos tupqt
    pure $ qt { AST.qtypeType = t }
applyTv2QType2 mod _ qt@AST.QType{ AST.qtypeType = AST.TVar pos _ []} AST.QType{ AST.qtypeType = AST.FuncType _ args ret} = do
    -- ex: apply func (a, b) -> c to `d
    let t = AST.FuncType pos args ret
    pure $ qt { AST.qtypeType = t }
applyTv2QType2 mod _ qt@AST.QType{ AST.qtypeType = AST.TVar{}} AST.QType{ AST.qtypeType = AST.VoidType} =
    -- ex: apply void to `a
    pure $ qt { AST.qtypeType = AST.VoidType }
applyTv2QType2 mod _ (AST.QType pos _ _) _ = do
    -- error:
    --   ex: apply `a<b, c> to `d of `d<e, f>
    --       apply a<b, c> to `d of `d<e, f>
    --       apply [a] to `b of `b<c, d>
    --       apply (a, b) to `c of `c<d, e>
    --       func (a, b) -> c to `d of `d<e, f>
    s <- S.get
    let msg  = errMsg (mod2file mod) pos "type mismatch"
        msgs = errMsgStack $ posStack s
    fail $ msg ++ msgs

getTV2QType tvs qts | length tvs == length qts = do
    let z = L.zipWith fun tvs qts
    pure $ MAP.fromList z
    where
        fun tv qt = (getIdTVK tv, qt)
getTV2QType _ _ = fail "internal error: the number of arguments is incompatible"

getIdTVK (AST.TypeVarKind _ ident _) = ident

astKind2TypeKind mod pos (AST.KV _) = fail $ errMsg (mod2file mod) pos "could not convert kind"
astKind2TypeKind _ _ AST.KStar      = pure Star
astKind2TypeKind mod pos (AST.KArray l r) = do
    l' <- astKind2TypeKind mod pos l
    r' <- astKind2TypeKind mod pos r
    pure $ Kfun l' r'

named2Type mod args (NamedData d)   = data2Type mod args d
named2Type mod args (NamedStruct d) = struct2Type mod args d
named2Type _ _ _                    = fail "internal error"

getQualType Nothing t     = t
getQualType (Just qual) t = TAp (TCon tc) t
    where
        tc    = Tycon ident k
        ident = case qual of
            AST.Shared -> PathID Nothing "shared"
            AST.Uniq   -> PathID Nothing "uniq"
        k     = Kfun Star Star

data2Type mod args (AST.Data pos (AST.IDPos _ ident) tvk _ _) = toType mod args pos ident tvk
struct2Type mod args (AST.Struct pos (AST.IDPos _ ident) tvk _ _) = toType mod args pos ident tvk

toType mod args pos ident tvk = do
    k  <- getKindTVK tvk
    k' <- astKind2TypeKind mod pos k
    let ident' = PathID (Just $ mod2file mod) ident
        t      = TCon (Tycon ident' k')
    pure $ foldl TAp t args

findObjFromRelPath :: File2Mod -> LModule -> [String] -> Maybe (Maybe FP.FilePath, LModule, Named)
findObjFromRelPath dict mod [ident]
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
findObjFromRelPath dict mod ident =
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

findObjFromABSPath f2m (Just fp) name =
    case MAP.lookup fp f2m of
        Just (mod, dict) ->
            case MAP.lookup name dict of
                Just n  -> Just (mod, n)
                Nothing -> Nothing
        Nothing -> Nothing
findObjFromABSPath f2m _ name
    | SET.member name primitiveTypes = Just (emptyLModule, NamedPrim name)
    | otherwise = Nothing

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
checkKindTop mod (AST.QType _ _ (AST.IDType ident@AST.IDAbs{} [])) = do
    (sbst, dict, _) <- S.get
    case findObjFromABSPath dict fp name of
        Nothing -> fail $ err "unknown type specifier"
        Just (_, n) -> do
            k <- getKind mod pos n
            case unifyKind [(k, AST.KStar)] of
                Nothing -> fail $ err "kind must be *"
                _       -> pure AST.KStar
    where
        fp   = AST.idAbsFile ident
        name = AST.idAbsID ident
        pos  = AST.idAbsPos ident
        err  = errMsg (mod2file mod) pos
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
checkKindIn mod (AST.QType _ qual (AST.IDType ident@AST.IDAbs{} qt)) = do
    (sbst, dict, _) <- S.get
    case findObjFromABSPath dict fp name of
        Nothing        -> fail $ err "unknown type specifier"
        Just (_, n) -> do
            k <- getKind mod pos n
            case qt of
                [] -> checkQual (applySbstKindArr sbst k) qual
                _  -> checkKindUnify mod pos k qt
    where
        fp   = AST.idAbsFile ident
        name = AST.idAbsID ident
        pos  = AST.idAbsPos ident
        err  = errMsg (mod2file mod) pos
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
        ks _                              = fail "internal error: getKindTVK"
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

{-
    resolve ID
-}
resolveLType mod f2m (AST.IDType ident@AST.IDRel{} qts) =
    case findObjFromRelPath f2m mod ident' of
        Just (fp, _, nm) -> do
            objid <- getTypeIdOfNamed mod pos nm
            qts'  <- mapM (resolveQType mod f2m) qts
            pure $ AST.IDType (AST.IDAbs pos ident' fp objid) qts'
        Nothing -> fail $ errMsg (mod2file mod) pos "no such type"
    where
        pos    = AST.idRelPos ident
        ident' = AST.idRelID ident
resolveLType mod f2m (AST.TVar pos ident qts) = do
    qts' <- mapM (resolveQType mod f2m) qts
    pure $ AST.TVar pos ident qts'
resolveLType mod f2m (AST.ArrayType pos qt exp) = do
    qt' <- resolveQType mod f2m qt
    pure $ AST.ArrayType pos qt' exp
resolveLType mod f2m (AST.TupleType pos qts) = do
    qts' <- mapM (resolveQType mod f2m) qts
    pure $ AST.TupleType pos qts'
resolveLType mod f2m (AST.FuncType pos args ret) = do
    args' <- mapM (resolveQType mod f2m) args
    ret'  <- resolveQType mod f2m ret
    pure $ AST.FuncType pos args' ret'
resolveLType _ _ v = pure v

getTypeIdOfNamed mod pos (NamedStruct s) = pure $ AST.idPosID (AST.structID s)
getTypeIdOfNamed mod pos (NamedData d)   = pure $ AST.idPosID (AST.dataID d)
getTypeIdOfNamed mod pos (NamedPrim i)   = pure i
getTypeIdOfNamed mod pos _               = fail $ errMsg (mod2file mod) pos "must be primitive, struct or data type"

resolveQType mod f2m qt = do
    t <- resolveLType mod f2m (AST.qtypeType qt)
    pure $ qt { AST.qtypeType = t }

getClassIdOfNamed _ _ (NamedClass c) = pure $ AST.idPosID (AST.classID c)
getClassIdOfNamed mod pos _          = fail $ errMsg (mod2file mod) pos "must be class name"

resolvePred mod f2m (AST.Pred pos ident@AST.IDRel{} qt) =
    case findObjFromRelPath f2m mod ident' of
        Just (fp, _, nm) -> do
            clsid <- getClassIdOfNamed mod pos nm
            qt'   <- resolveQType mod f2m qt
            pure $ AST.Pred pos (AST.IDAbs idpos ident' fp clsid) qt'
        Nothing -> fail $ errMsg (mod2file mod) pos "no such class"
    where
        ident' = AST.idRelID ident
        idpos  = AST.idRelPos ident
resolvePred _ _ pred = pure pred

resolveStruct mod f2m s = do
    pred <- mapM (resolvePred mod f2m) (AST.structPred s)
    mem  <- mapM (resolveProdMem mod f2m) (AST.structMem s)
    pure $ s { AST.structPred = pred, AST.structMem = mem }

resolveProdMem mod f2m mem = do
    qt <- resolveQType mod f2m (AST.prodMemQType mem)
    pure $ mem { AST.prodMemQType = qt }

resolveData mod f2m d = do
    pred <- mapM (resolvePred mod f2m) (AST.dataPred d)
    mem  <- mapM (resolveSumMem mod f2m) (AST.dataMem d)
    pure $ d { AST.dataPred = pred, AST.dataMem = mem }

resolveSumMem mod f2m mem = do
    inmem <- mapM (resolveQType mod f2m) (AST.sumMemQType mem)
    pure $ mem { AST.sumMemQType = inmem }

resolveClass mod f2m cls = do
    pred <- mapM (resolvePred mod f2m) (AST.classPred cls)
    intf <- mapM (resolveInterface mod f2m) (AST.classIF cls)
    pure $ cls { AST.classPred = pred, AST.classIF = intf }

resolveInterface mod f2m intf = do
    t <- resolveLType mod f2m (AST.ifType intf)
    pure $ intf { AST.ifType = t }

resolveInstance mod f2m inst = do
    head  <- resolvePred mod f2m (AST.instHead inst)
    pred  <- mapM (resolvePred mod f2m) (AST.instPred inst)
    defun <- mapM (resolveDefun mod f2m) (AST.instDefun inst)
    pure $ inst { AST.instHead = head, AST.instPred = pred, AST.instDefun = defun }

resolveDefun mod f2m defun = do
    ret <- case AST.defunRet defun of
        Just qt -> Just <$> resolveQType mod f2m qt
        Nothing -> pure Nothing
    pred <- mapM (resolvePred mod f2m) (AST.defunPred defun)
    args <- mapM (resolveArg mod f2m) (AST.defunArgs defun)
    -- TODO: expr
    pure $ defun { AST.defunRet = ret, AST.defunPred = pred, AST.defunArgs = args }

resolveArg mod f2m arg = do
    t <- case AST.argQType arg of
        Just qt -> Just <$> resolveQType mod f2m qt
        Nothing -> pure Nothing
    pure $ arg { AST.argQType = t }

resolveNamed mod f2m (NamedStruct s)      = NamedStruct <$> resolveStruct mod f2m s
resolveNamed mod f2m (NamedData d)        = NamedData <$> resolveData mod f2m d
resolveNamed mod f2m (NamedClass c)       = NamedClass <$> resolveClass mod f2m c
resolveNamed mod f2m (NamedSum ident mem) = NamedSum ident <$> resolveSumMem mod f2m mem
resolveNamed mod f2m (NamedFunc f)        = NamedFunc <$> resolveDefun mod f2m f
resolveNamed _ _ p                        = pure p

resolveDictIn f2m (mod, dict) = do
    dict' <- mapM (resolveNamed mod f2m) dict
    pure (mod, dict')

resolve f2m = mapM (resolveDictIn f2m) f2m
