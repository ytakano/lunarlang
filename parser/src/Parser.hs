{-# LANGUAGE FlexibleContexts #-}

module Parser (
    parse
) where

import qualified AST
import           Control.Applicative
import           Data.Char
import           Data.Functor
import           Data.Maybe
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
term = parens expr <|> literal <?> "term"

whiteSpacesNotEOL =
    P.many $ P.satisfy isSpace'
    where
        isSpace' c = isSpace c && c /= '\r' && c /= '\n'

lineComment = (P.string "//" >> P.many (P.satisfy (`notElem` "\r\n"))) <|> pure ""

eol = P.oneOf ";\r\n"

whiteSpaceWTSC = do
    whiteSpace
    P.lookAhead (P.satisfy ((/=) ';') $> ())
        <|> (P.many1 (P.char ';') >> whiteSpaceWTSC)

exprs = do
    whiteSpace
    e <- expr
    P.try (whiteSpaceWTSC >> P.lookAhead (P.char '}') $> [e]) <|> exprs' [e]
    where
        exprs' es = do
            whiteSpacesNotEOL
            lineComment
            eol
            whiteSpaceWTSC
            e <- expr
            P.try (whiteSpaceWTSC >> P.lookAhead (P.char '}') $> reverse (e:es))
                <|> exprs' (e:es)

lexer      = T.makeTokenParser def
reservedOp = T.reservedOp lexer
natural    = T.natural lexer
parens     = T.parens lexer
braces     = T.braces lexer
angles     = T.angles lexer
brackets   = T.brackets lexer
identifier = T.identifier lexer
reserved   = T.reserved lexer
whiteSpace = T.whiteSpace lexer
commaSep   = T.commaSep lexer
commaSep1  = T.commaSep1 lexer

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
    $ARG      := $ID $TYPESPEC?
    $TYPESPEC := : $QTYPE
-}
arg = do
    pos <- getPos
    id <- identifier
    whiteSpace
    qt <- (P.char ':' >> whiteSpace >> Just <$> qtype)
        <|> pure Nothing
    pure $ AST.Arg pos id qt

{-
    $QTYPE   := $QUALIFIER? $TYPE | $TVAR <$QTYPES>?
    $TYPE    := $CSID <$QTYPES>? | func ( $QTYPES? ) $RETTYPE |
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
    $CSID <$QTYPES>? | func ( $QTYPES? ) $RETTYPE
-}
typeFuncID = do
    pos <- getPos
    id <- P.try (reserved "func" $> "func") <|> identifier
    case id of
        "func" -> typeFunc pos
        _      -> typeID pos id

{-
    $CSID <$QTYPES>?
-}
typeID pos id = do
    id' <- csid2 [id]
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
    $CSID := $ID | $ID : $CSID
-}
csid = do
    h <- identifier
    ids <- csid2 [h]
    return ids

{-
    (: $ID)*
-}
csid2 ids = whiteSpace >> csid2' <|> pure (reverse ids)
    where
        csid2' = do
            P.char ':'
            whiteSpace
            id <- identifier
            csid2 $ id:ids

{-
    $TVAR := `$ID
-}
tvar = do
    P.char '`'
    id <- identifier
    pure $ '`':id

{-
    $PRED := $CSID <$QTYPE>
-}
predicate = do
    pos <- getPos
    id <- csid
    whiteSpace
    qt <- angles qtype
    pure $ AST.Pred pos id qt

-- TODO
literal = do
    pos <- getPos
    num <- natural
    pure $ AST.ExprLiteral pos (AST.LitInt num)
