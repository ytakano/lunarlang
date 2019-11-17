{-# LANGUAGE FlexibleContexts #-}

module Parser (
    parse
) where

import qualified AST
import           Control.Applicative
import           Data.Char
import           Data.Functor
import           Data.Maybe
import           Debug.Trace
import           Text.Parsec          ((<?>))
import qualified Text.Parsec          as P
import qualified Text.Parsec.Expr     as E
import           Text.Parsec.Language as L
import qualified Text.Parsec.Token    as T

getPos = getPos' <$> P.getPosition
    where
        getPos' pos = AST.Pos (P.sourceLine pos) (P.sourceColumn pos)

parse = P.parse $ statements []

-- letter | digit | multibyte character
parseChar :: P.Parsec String () Char
parseChar = P.digit <|> parseNonum

-- letter | multibyte character
parseNonum :: P.Parsec String () Char
parseNonum = P.letter <|> P.noneOf ['\0'..'\127']

expr = E.buildExpressionParser table term <?> "expression"

table = [[prefix "-" (opPrefix "-")],
         [binary "*" (opBin "*") E.AssocLeft,
          binary "/" (opBin "/") E.AssocLeft,
          binary "%" (opBin "%") E.AssocLeft],
         [binary "+" (opBin "+") E.AssocLeft,
          binary "-" (opBin "-") E.AssocLeft],
         [binary "<<" (opBin "<<") E.AssocLeft,
          binary ">>" (opBin ">>") E.AssocLeft],
         [binary "<" (opBin "<") E.AssocLeft,
          binary ">" (opBin ">") E.AssocLeft,
          binary "<=" (opBin "<=") E.AssocLeft,
          binary ">=" (opBin ">=") E.AssocLeft],
         [binary "==" (opBin "==") E.AssocLeft,
          binary "!=" (opBin "!=") E.AssocLeft]]
    where
        opPrefix = AST.ExprPrefix
        opBin = AST.ExprBin
        binary name fun assoc = E.Infix (do{ reservedOp name; pure fun }) assoc
        prefix name fun = E.Prefix (do{ reservedOp name; pure fun })

term = do
    e <- term'
    whiteSpace
    apply' e <|> indexing e <|> pure e

{-
    $EXPR1   := $DOTID | $IF | $LET | ( $EXPR ) | $TUPLE |
                { $EXPRS } | [ $EXPRS'? ] | $LITERAL
-}
term' = exprs2
    <|> parenTuple
    <|> array
    <|> literal
    <|> let'
    <|> if'
    <|> match
    <|> exprDotID <?> "term"

whiteSpaceWTSC = do
    whiteSpace
    P.lookAhead (P.satisfy ((/=) ';') $> ())
        <|> (P.many1 (P.char ';') >> whiteSpaceWTSC)

exprs = do
    whiteSpaceWTSC
    e <- expr
    P.try (whiteSpaceWTSC >> P.lookAhead (P.char '}') $> [e]) <|> exprs' [e]
    where
        exprs' es = do
            whiteSpaceWTSC
            e <- expr
            P.try (whiteSpaceWTSC >> P.lookAhead (P.char '}') $> reverse (e:es))
                <|> exprs' (e:es)

{-
    { $EXPRS }
-}
exprs2 = do
    AST.Exprs <$> getPos <*> braces (exprs <* whiteSpace)

lexer         = T.makeTokenParser def
reservedOp    = T.reservedOp lexer
natural       = T.natural lexer
float         = T.float lexer
parens        = T.parens lexer
braces        = T.braces lexer
angles        = T.angles lexer
brackets      = T.brackets lexer
identifier    = T.identifier lexer
reserved      = T.reserved lexer
whiteSpace    = T.whiteSpace lexer
commaSep      = T.commaSep lexer
commaSep1     = T.commaSep1 lexer
stringLiteral = T.stringLiteral lexer
charLiteral   = T.charLiteral lexer
operator      = T.operator lexer

def = L.emptyDef{T.commentLine = "//",
                 T.identStart = parseNonum,
                 T.identLetter = parseChar,
                 T.opStart = P.oneOf "*/%+-<>=!",
                 T.opLetter = P.oneOf "*/%+-<>=!",
                 T.reservedOpNames = ["+",  "-",  "/", "*", "%",
                                      "<<", ">>", "<", ">", "<=",
                                      ">=", "==", "!="],
                 T.reservedNames = ["true", "false", "void",
                                    "class", "instance", "struct", "data",
                                    "if", "elif", "else",
                                    "let", "in", "func", "require",
                                    "match", "import", "as", "here",
                                    "prefix", "infix", "shared", "uniq"]}

statement = do
    pos <- getPos
    (reserved "func" >> defun pos)
        <|> (reserved "data" >> dataDef pos)
        <|> (reserved "class" >> classDef pos)
        <|> (reserved "instance" >> instance' pos)
        <|> (reserved "import" >> import' pos)
        <|> (reserved "struct" >> struct pos)
        <?> "func, data, class or instance"

statements t = do
    whiteSpace
    h <- statement
    whiteSpace
    P.eof $> reverse (h:t) <|> statements (h:t)

{-
    $DEFUN  := func $ID ( $ARGS? ) $RETTYPE? $PREDS? { $EXPRS }
-}
defun pos = do
    whiteSpace
    posid <- getPos
    id <- identifier
    whiteSpace
    args <- parens $ commaSep arg
    whiteSpace
    ret <- (Just <$> retType) <|> pure Nothing
    whiteSpace
    preds <- predicates
    e <- braces exprs
    pure $ AST.TOPDefun (AST.Defun pos (AST.IDPos posid id) args ret preds e)

{-
    $ARG      := $PATTERN $TYPESPEC?
    $TYPESPEC := : $QTYPE
-}
arg = do
    pos <- getPos
    pat <- dbind
    whiteSpace
    qt <- (P.char ':' >> whiteSpace >> Just <$> qtype)
        <|> pure Nothing
    pure $ AST.Arg pos pat qt

{-
    $QTYPE   := $QUALIFIER? $TYPE | $TVAR <$QTYPES>?
    $TYPE    := $DOTID <$QTYPES>? | func ( $QTYPES? ) $RETTYPE |
                ( $QTYPES? ) | [ $QTYPE ]
-}
qtype = do
    pos <- getPos
    AST.QType pos Nothing <$> typeTvar <|> qtype' pos

qtype' pos = do
    qual <- P.try (
        (reserved "shared" >> whiteSpace $> Just AST.Shared)
        <|> (reserved "uniq" >> whiteSpace $> Just AST.Uniq)
        <|> pure Nothing)
    t <- typeTuple <|> typeArray <|> typeFuncID
    pure $ AST.QType pos Nothing t

{-
    ( $QTYPES? )
-}
typeTuple = do
    pos <- getPos
    qt <- parens $ commaSep qtype <* whiteSpace
    pure $ AST.TupleType pos qt

{-
    [ $QTYPE ]
-}
typeArray = do
    pos <- getPos
    qt <- brackets $ qtype <* whiteSpace
    pure $ AST.ArrayType pos qt

{-
    $DOTID <$QTYPES>? | func ( $QTYPES? ) $RETTYPE
-}
typeFuncID = do
    pos <- getPos
    id <- P.try (reserved "func" $> "func") <|> identifier
    case id of
        "func" -> typeFunc pos
        _      -> typeID pos id

{-
    $DOTID <$QTYPES>?
-}
typeID pos id = do
    id' <- dotid2 [id]
    whiteSpace
    ta <- angles (commaSep1 qtype <* whiteSpace) <|> pure []
    pure $ AST.IDType pos id' ta

{-
    ( $QTYPES? ) $RETTYPE
-}
typeFunc pos = do
    whiteSpace
    args <- parens $ commaSep qtype <* whiteSpace
    whiteSpace
    ret <- retType
    pure $ AST.FuncType pos args ret

{-
    $RETTYPE := -> $QTYPE
-}
retType = do
    P.string "->"
    whiteSpace
    qtype

{-
    $TVAR <$QTYPES>?
-}
typeTvar = do
    pos <- getPos
    id <- tvar
    whiteSpace
    args <- typeArgs
    pure $ AST.TVar pos id args

{-
    <$QTYPES>?
-}
typeArgs = angles (commaSep1 qtype <* whiteSpace) <|> pure []

{-
    $DOTID := $ID | $ID : $DOTID
-}
dotid = do
    h <- identifier
    ids <- dotid2 [h]
    pure ids

{-
    (. $ID)*
-}
dotid2 ids =
    (P.try (whiteSpace >> P.char '.') >> dotid2') <|> pure (reverse ids)
    where
        dotid2' = do
            whiteSpace
            id <- identifier
            dotid2 $ id:ids

{-
    $TVAR := `$ID
-}
tvar = do
    P.char '`'
    id <- identifier
    pure $ '`':id

{-
    $PRED := $DOTID <$QTYPE>
-}
predicate = do
    pos <- getPos
    id <- dotid
    whiteSpace
    qt <- angles qtype
    pure $ AST.Pred pos id qt

predicates = (reserved "require" >> whiteSpace >> commaSep1 predicate) <|> pure []

{-
    $LITERAL := $STR | $CHAR | $FLOAT | $NATURAL
-}
literal = AST.ExprLiteral <$> getPos <*> literal'

literal' =
    AST.LitStr <$> stringLiteral
        <|> AST.LitChar <$> charLiteral
        <|> P.try (AST.LitFloat <$> float)
        <|> AST.LitInt <$> natural
        <|> AST.LitBool <$> ((reserved "true" $> True) <|> (reserved "false" $> False))
        <?> "literal"

{-
    $DBIND   := _ | $DBINDSP | $DOTID $DBINDSP?
    $DBINDS  := $DBIND | $DBIND , $DBINDS
    $DBINDSP := ( $DBINDS )
-}
dbind = do
    pos <- getPos
    (P.char '_' $> AST.DBindIgnore pos)
        <|> dbindsP pos
        <|> dbinds pos

{-
    ( $DBINDS )
-}
dbindsP pos =
    AST.DBindTuple pos <$> parens (commaSep1 dbind <* whiteSpace)

{-
    $DOTID $DBINDSP?
-}
dbinds pos = AST.DBindDotID pos <$> dotid <*> dbinds2

{-
    ( $DBINDS )
-}
dbinds2 =
    (P.try (whiteSpace >> P.lookAhead (P.char '(')) >> dbinds2')
        <|> pure Nothing
    where
        dbinds2' = Just <$> parens (commaSep1 dbind <* whiteSpace)

{-
    $DBIND $TYPESPEC? = $EXPR
-}
binding = do
    pos <- getPos
    b <- dbind
    whiteSpace
    qt <- (P.char ':' >> whiteSpace >> (Just <$> qtype) <* whiteSpace) <|> pure Nothing
    P.char '='
    whiteSpace
    e <- expr
    pure (b, qt, e)

{-
    $LET   := let $BINDS
    $BIND  := $DBIND $TYPESPEC? = $EXPR
    $BINDS := $BIND | $BIND , $BINDS
-}
let' = do
    pos <- getPos
    reserved "let"
    whiteSpace
    AST.ExprLet pos <$> commaSep1 binding

{-
    $IF := if $EXPR { $EXPRS } $ELSE?
-}
if' = do
    pos <- getPos
    reserved "if"
    whiteSpace
    cond <- expr
    es <- braces (exprs <* whiteSpace)
    r <- elifelse1
    pure $ AST.ExprIf pos cond es r

{-
    $ELSE?
-}
elifelse1 = do
    whiteSpace
    pos <- getPos
    (P.try (reserved "elif") >> Just <$> elif pos)
        <|> (reserved "else" >> Just <$> else' pos)
        <|> pure Nothing

{-
    $ELSE := elif $EXPR { $EXPRS } $ELSE | else { $EXPRS }
-}
elifelse2 = do
    whiteSpace
    pos <- getPos
    (P.try (reserved "elif") >> elif pos) <|> (reserved "else" >> else' pos) <?> "elif or else"

{-
    { $EXPRS } $ELSE
-}
elif pos = AST.Elif pos <$> expr <*> braces (exprs <* whiteSpace) <*> elifelse2

{-
    { $EXPRS }
-}
else' pos = AST.Else pos <$> braces (exprs <* whiteSpace)

exprDotID = AST.ExprDotID <$> getPos <*> dotid

{-
    ( $EXPR )
    $TUPLE := ( $EXPR, ) | ( $EXPRS'? )
-}
parenTuple = do
    pos <- getPos
    P.char '('
    whiteSpace
    (P.char ')' $> AST.ExprTuple pos []) <|> parenTuple' pos

parenTuple' pos = do
    e <- expr
    whiteSpace
    P.char ')' $> e <|> tuple pos e

tuple pos e = do
    P.char ','
    whiteSpace
    (P.char ')' $> AST.ExprTuple pos [e]) <|> tuple' pos e

tuple' pos e = do
    es <- commaSep (expr <* whiteSpace)
    P.char ')'
    pure $ AST.ExprTuple pos (e:es)

{-
    [ $EXPRS'? ]
-}
array =
    AST.ExprArray <$> getPos <*> brackets (commaSep expr <* whiteSpace)

{-
    $APPLY $EXPR2
    $APPLY := ( $EXPRS'? )
-}
apply' e = do
    e' <- AST.ExprApply e <$> parens (commaSep expr <* whiteSpace)
    expr0 e'

{-
    [ $EXPR ] $EXPR2
-}
indexing e = do
    e' <- AST.ExprIndex e <$> brackets (expr <* whiteSpace)
    expr0 e'

{-
    $EXPR0 := $EXPR1 $EXPR2
-}
expr0 e = do
    whiteSpace
    apply' e <|> indexing e <|> pure e

{-
    $MATCH := match expr { $PATEXPR+ }
-}
match = do
    pos <- getPos
    reserved "match"
    whiteSpace
    e <- expr
    whiteSpace
    AST.ExprMatch pos e <$> braces (P.many1 patexpr <* whiteSpace)

{-
    $PATEXPR := $PATTERN in $EXPR
-}
patexpr = do
    whiteSpace
    p <- pattern'
    whiteSpace
    reserved "in"
    whiteSpace
    e <- expr
    pure (p, e)

{-
    $PATTERN := _ | $PATTERNP | $DOTID $PATTERNP? | $LITERAL
-}
pattern' = do
    pos <- getPos
    whiteSpace
    (P.char '_' $> AST.PatIgnore pos)
        <|> patternP pos
        <|> patternLit pos
        <|> patternID pos

{-
    $PATTERNP := ( $PATTERNS )
-}
patternP pos = AST.PatTuple pos <$> parens (commaSep pattern' <* whiteSpace)

patternLit pos = AST.PatLiteral pos <$> literal'

patternID pos = AST.PatDotID pos <$> dotid <*> (patexpr <|> pure Nothing)
    where
        patexpr = do
            P.try (whiteSpace >> P.lookAhead (P.char '('))
            Just <$> parens (commaSep pattern' <* whiteSpace)

typeArg =
    AST.TypeVar <$> getPos <*> tvar

{-
    $data     := data $ID $TVARS? $PREDS? { $SUM }
    $SUM      := $SUMTYPE | $SUMTYPE $SEP $SUM
    $SUMTYPE  := $ID $SUMTYPES?
    $SUMTYPES := : $QTYPE | : ($QTYPES)
-}
dataDef pos = do
    whiteSpace
    posid <- getPos
    id <- identifier
    whiteSpace
    ta <- angles (commaSep1 tvarKind <* whiteSpace) <|> pure []
    whiteSpace
    preds <- predicates
    whiteSpace
    P.char '{'
    mem <- dataMembers []
    pure $ AST.TOPData (AST.Data pos (AST.IDPos posid id) ta preds mem)

dataMember = do
    pos <- getPos
    id <- identifier
    whiteSpace
    qt <- (P.char ':' >> (whiteSpace >> (parens (commaSep1 qtype <* whiteSpace) <|> (:[]) <$> qtype)))
        <|> pure []
    pure $ AST.SumMem pos id qt

dataMembers mem = do
    whiteSpaceWTSC
    (P.char '}' $> reverse mem)
        <|> (do
            m <- dataMember
            dataMembers (m:mem))

{-
    $CLASSDECL := class $ID < $TVARKIND > $PREDS? { $INTERFACES }
-}
classDef pos = do
    whiteSpace
    posid <- getPos
    id <- identifier
    whiteSpace
    tv <- angles (commaSep1 tvarKind <* whiteSpace)
    whiteSpace
    preds <- predicates
    whiteSpace
    is <- interfaces
    pure $ AST.TOPClassDef (AST.ClassDef pos (AST.IDPos posid id) tv preds is)

{-
    $TVAR      := `$ID
    $TVARKIND  := `$ID | `$ID :: $KIND
-}
tvarKind = do
    pos <- getPos
    tv <- tvar
    k <- (P.try (whiteSpace >> P.string "::") >> whiteSpace >> Just <$> kind) <|> pure Nothing
    pure $ AST.TypeVarKind pos tv k

{-
    $KIND := $STAR | $STAR -> $KIND
    $STAR := *
-}
kind = do
    lhs <- P.char '*' $> AST.KStar
    (P.try (whiteSpace >> P.string "->") >> whiteSpace >> (AST.KArroy lhs <$> kind))
        <|> pure AST.KStar

{-
    $INTNAME := $ID | infix $INFIX | prefix $PREFIX
-}
intname = do
    pos <- getPos
    (reserved "infix" >> whiteSpace >> AST.IntInfix pos <$> operator)
        <|> (reserved "prefix" >> whiteSpace >> AST.IntPrefix pos <$> operator)
        <|> AST.IntFunc pos <$> identifier

{-
    func ( $QTYPES? ) $RETTYPE
-}
inttype = do
    pos <- getPos
    reserved "func"
    whiteSpace
    typeFunc pos

{-
    $INTERFACE  := $INTNAMES :: func ( $QTYPES? ) $RETTYPE
-}
interface = do
    names <- commaSep1 intname
    whiteSpace
    P.string "::"
    whiteSpace
    t <- inttype
    pure $ AST.Interface names t

interfaces = do
    P.char '{'
    whiteSpaceWTSC
    i <- interface
    whiteSpaceWTSC
    (P.char '}' $> [i]) <|> interfaces' [i]
    where
        interfaces' is = do
            whiteSpaceWTSC
            i <- interface
            whiteSpaceWTSC
            (P.char '}' $> reverse (i:is)) <|> interfaces' (i:is)

{-
    $INST := instance $PRED $PREDS? { $DEFUNS }
-}
instance' pos = do
    whiteSpace
    p <- predicate
    whiteSpace
    ps <- predicates
    whiteSpace
    P.char '{'
    fs <- instDefuns
    pure $ AST.TOPInstance (AST.Instance pos p ps fs)

instDefuns = do
    whiteSpace
    f <- instDefun
    instDefuns' [f]
    where
        instDefuns' fs = do
            whiteSpace
            (P.char '}' $> reverse fs) <|>
                (do
                    f <- instDefun
                    instDefuns' (f:fs))

instDefun = do
    pos <- getPos
    reserved "func"
    whiteSpace
    posid <- getPos
    id <- (P.try (reserved "infix") >> whiteSpace >> operator)
        <|> identifier
    whiteSpace
    args <- parens $ commaSep arg
    whiteSpace
    ret <- (Just <$> retType) <|> pure Nothing
    whiteSpace
    preds <- predicates
    e <- braces exprs
    pure $ AST.Defun pos (AST.IDPos posid id) args ret preds e

{-
    $IMPORT := import $DOTID $HEREAS?
    $HEREAS := here | $AS
    $AS := as $ID
-}
import' pos = do
    whiteSpace
    id <- dotid
    h <- (P.try (whiteSpace >> reserved "here") $> AST.ImportHere)
        <|> (P.try (whiteSpace >> reserved "as") >> whiteSpace >> AST.ImportAs <$> dotid)
        <|> pure AST.ImportNS
    pure $ AST.TOPImport (AST.Import pos id h)

{-
    $STRUCT   := struct $ID $TVARS? $PREDS? { $PROD }
    $PROD     := $PRODTYPE | $PRODTYPE $SEP $PROD
    $PRODTYPE := $ID $TYPESPEC
-}
struct pos = do
    whiteSpace
    posid <- getPos
    id <- identifier
    whiteSpace
    tv <- angles (commaSep1 tvarKind <* whiteSpace) <|> pure []
    whiteSpace
    p <- predicates
    whiteSpace
    mem <- prodTypes
    pure $ AST.TOPStruct (AST.Struct pos (AST.IDPos posid id) tv p mem)

prodTypes = do
    P.char '{'
    whiteSpace
    h <- prodMem
    whiteSpace
    (P.char '}' $> [h]) <|> prodTypes' [h]
    where
        prodTypes' t = do
            h <- prodMem
            whiteSpace
            (P.char '}' $> reverse (h:t)) <|> prodTypes' (h:t)

prodMem = do
    pos <- getPos
    id <- identifier
    whiteSpace
    P.char ':'
    whiteSpace
    t <- qtype
    pure $ AST.ProdMem pos id t
