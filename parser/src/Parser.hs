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

parse = P.parse statement

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

-- TODO
term = parens expr <|> literal <|> let' <?> "term"

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

def = L.emptyDef{T.commentLine = "//",
                 T.identStart = parseNonum,
                 T.identLetter = parseChar,
                 T.opStart = P.oneOf "*/%+-<>=!",
                 T.opLetter = P.oneOf "*/%+-<>=!",
                 T.reservedOpNames = ["+",  "-",  "/", "*", "%",
                                      "<<", ">>", "<", ">", "<=",
                                      ">=", "==", "!="],
                 T.reservedNames = ["true", "false", "void",
                                    "class", "instance", "data", "memory",
                                    "if", "elif", "else",
                                    "let", "in", "func", "require",
                                    "match", "import", "as", "here",
                                    "prefix", "infix", "shared", "uniq"]}

statement = do
    pos <- getPos
    (reserved "func" >> defun pos)
        <|> reserved "class" $> AST.Class
        <|> reserved "instance" $> AST.Inst

{-
    $DEFUN  := func $ID ( $ARGS? ) $RETTYPE? $PREDS? { $EXPRS }
-}
defun pos = do
    whiteSpace
    id <- identifier
    whiteSpace
    args <- parens $ commaSep arg
    whiteSpace
    ret <- (Just <$> retType) <|> pure Nothing
    whiteSpace
    preds <- (reserved "require" >> whiteSpace >> commaSep1 predicate)
        <|> pure []
    e <- braces exprs
    pure $ AST.Defun $ AST.Fun pos id args ret preds e

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
    [ $QTYPES ]
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
    pure $ AST.IDType pos id' []

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
    return ids

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
{-
    $LITERAL := $STR | $CHAR | $FLOAT | $NATURAL
-}
literal = do
    pos <- getPos
    lit pos . AST.LitStr <$> stringLiteral
        <|> lit pos . AST.LitChar <$> charLiteral
        <|> P.try (lit pos . AST.LitFloat <$> float)
        <|> lit pos . AST.LitInt <$> natural
        <?> "literal"
    where
        lit = AST.ExprLiteral

{-
    $DBIND   := _ | $DBINDSP | $DOTID $DBINDSP?
    $DBINDS  := $DBIND | $DBIND , $DBINDS
    $DBINDSP := ( $DBINDS )
-}
dbind = do
    pos <- getPos
    P.char '_' $> AST.DBindIgnore pos
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
