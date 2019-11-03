{-# LANGUAGE FlexibleContexts #-}

module Parser (
    parse
) where

import qualified AST
import           Control.Applicative
import           Data.Maybe
import           Text.Parsec                            ((<?>))
import qualified Text.Parsec                            as P
import qualified Text.Parsec.Expr                       as E
import qualified Text.Parsec.Token                      as T
import           Text.ParserCombinators.Parsec.Language (haskellDef)

parse = P.parse parseTopID

parseTopID = do
    parseSpaces
    id <- P.try (P.string "class") <|> P.string "func"
    parseTop id

parseTop "class" = return AST.Class
parseTop "func"  = parseDefun
parseTop _       = fail "invalid identifier"

parseID = do
    h <- parseNonum
    t <- P.many parseChar
    return (h:t)

-- letter | digit | multibyte character
parseChar :: P.Parsec String () Char
parseChar = P.digit <|> parseNonum

-- letter | multibyte character
parseNonum :: P.Parsec String () Char
parseNonum = P.letter <|> P.noneOf ['\0'..'\127']

parseIsChar c = P.try (P.char c >> return True) <|> return False

-- $ID ( $ARGS? ) $TYPESPEC? $PREDS? { $EXPRS }
parseDefun = do
    parseSpacesPlus
    id <- parseID
    parseSpaces
    P.char '('
    parseSpaces

    -- parse arguments
    args <- parseArgs

    -- parse return value type
    parseSpaces
    c <- parseIsChar ':'
    parseSpaces
    ret <- if c then do
        t <- parseQType
        parseSpaces
        return $ Just t
    else
        return Nothing

    -- parse predicates
    preds <- parsePreds
    parseSpaces

    -- TODO: parse expressions
    P.char '{'
    parseSpaces
    e <- parseExpr
    parseSpaces
    P.char '}'

    return $ AST.Defun (AST.Fun id args ret preds [e])

parseComment = do
    P.string "//"
    P.many $ P.satisfy (`notElem` "\r\n")
    return ' '

parseSpacesPlus = do
    c <- P.try (P.satisfy (`elem` " \t\r\n")) <|> parseComment
    parseSpaces

parseSpaces = do
    P.many $ P.satisfy (`elem` " \t\r\n")
    P.try (do
        parseComment
        parseSpaces) <|> return Nothing

-- $PREDS  := require $PREDS_
-- $PREDS_ := $PRED | $PRED, $PRED
parsePreds = do
    c <- P.try (do
        P.string "require"
        parseSpacesPlus
        return True) <|> return False
    if c then do
        p <- parsePred
        parseSpaces
        preds_ [p]
    else
        return []
    where
        preds_ pds = do
            r <- parseIsChar ','
            if r then do
                parseSpaces
                p' <- parsePred
                parseSpaces
                preds_ $ p':pds
            else
                return $ reverse pds

-- $PRED := $CSID <$QTYPE>
parsePred = do
    id <- parseCSID
    parseSpaces
    P.char '<'
    parseSpaces
    t <- parseQType
    parseSpaces
    P.char '>'
    return $ AST.Pred id t

-- $QTYPE := $QUALIFIER? $TYPE | $TVAR <$QTYPES>?
parseQType = do
    c <- parseIsChar '`'
    if c then do
        t <- parseTVar
        return $ AST.QType Nothing t
    else do
        id <- parseID
        let q = if id == "shared" then AST.Shared else AST.Uniq
        t <- if id /= "shared" && id /= "uniq" then do
            parseSpaces
            parseTID id
        else do
            parseSpacesPlus
            parseType
        return $ AST.QType (Just q) t

-- $QTYPES := $QTYPE | $QTYPE , $QTYPES
parseQTypes = do
    h <- parseQType
    t <- qts []
    return $ h:t
    where
        qts qtypes = do
            _ <- parseSpaces
            c <- parseIsChar ','
            if c then do
                _ <- parseSpaces
                t <- parseQType
                qts $ t:qtypes
            else
                return $ reverse qtypes

-- $TYPE := $CSID <$QTYPES>? | func ( $QTYPES? ) $TYPESPEC | ( $QTYPES? ) | [ $QTYPE ]
parseType = do
    c <- P.try (P.char '(') <|> P.char '[' <|> parseNonum
    case c of
        '(' -> parseTupleType
        '[' -> parseArrayType
        _   -> do
            t <- P.try parseID <|> return ""
            let id = c:t
            -- TODO: parse function type
            parseTID id

-- $QTYPES>
parseTArgs = do
    parseSpaces
    t <- parseQTypes
    parseSpaces
    P.char '>'
    return t

-- $TVAR
parseTVar = do
    id <- parseID
    c <- parseIsChar '<'
    targs <- if c then parseTArgs else return []
    return $ AST.TVar ('`':id) targs

-- (: ID)* <$QTYPES>?
parseTID id = do
    ids <- parseCSID2 []
    let csid = id:ids
    c <- parseIsChar '<'
    targs <- if c then parseTArgs else return []
    return $ AST.IDType csid targs

-- $CSID := $ID | $ID : $CSID
parseCSID = do
    h <- parseID
    ids <- parseCSID2 [h]
    return ids

-- (: $ID)*
parseCSID2 ids = do
    parseSpaces
    c <- parseIsChar ':'
    if c then do
        parseSpaces
        id <- parseID
        parseCSID2 $ id:ids
    else
        return $ reverse ids

-- ( $QTYPES? )
parseTupleType = do
    parseSpaces
    t <- parseQTypes
    parseSpaces
    P.char ')'
    return $ AST.TupleType t

-- [ $QTYPE ]
parseArrayType = do
    parseSpaces
    t <- parseQType
    P.char ']'
    return $ AST.ArrayType t

-- $TYPESPEC := : $QTYPE
-- $ARG      := $ID $TYPESPEC?
parseArg = do
    id <- parseID
    parseSpaces
    c <- parseIsChar ':'
    t <- if c then do
        parseSpaces
        qt <- parseQType
        return $ Just qt
    else
        return Nothing
    return $ AST.Arg id t

-- ')' | $ARG ')' | $ARG (, $ARGS)* ')'
parseArgs = P.try (P.char ')' >> return []) <|> firstArg
    where
        firstArg = do
            h <- parseArg
            parseSpaces
            tailArgs [h]
        tailArgs args = do
            c <- parseIsChar ','
            if c then do
                parseSpaces
                h <- parseArg
                parseSpaces
                tailArgs $ h:args
            else do
                P.char ')'
                return $ reverse args

-- $DECIMAL := [1-9][0-9]* | 0
parseDecimal = do
    h <- P.oneOf ['1'..'9']
    t <- P.many P.digit
    return $ AST.LitInt (read (h:t))

parseLiteral = do
    d <- parseDecimal
    return $ AST.ExprLiteral d

-- TODO
-- $EXPR1 := $CSID | $IF | $TUPLE | $LITERAL
parseExpr1 = do
    id <- P.try (parseID >>= \x -> return $ Just x) <|> return Nothing
    case id of
        Just "if" -> parseIf
        Just id'  -> return $ AST.ExprCSID [id']
        Nothing   -> expr1'
    where
        expr1' = do
            c <- P.try (P.satisfy (`elem` "(") >>= \x -> return $ Just x) <|> return Nothing
            case c of
                Just '(' -> parseTuple
                Nothing  -> parseLiteral

-- $IF   := if $EXPR { $EXPRS } $ELSE?
-- $ELSE := elif $EXPR { $EXPRS } $ELSE | else { $EXPRS }
parseIf = do
    parseSpacesPlus
    -- TODO
    e1 <- parseExpr
    e2 <- parseExpr
    e3 <- parseExpr
    return $ AST.ExprIf e1 e2 e3

parseTuple = do
    parseSpaces
    -- TODO
    e <- parseExpr
    return $ AST.ExprTuple [e]

-- $EXPR0 := $EXPR1 $EXPR2
parseExpr0 = do
    e <- parseExpr1
    parseSpaces
    e' <- parseExpr2 e
    parseSpaces
    return $ fromMaybe e e'

-- TODO
-- $EXPR2 := ∅ | $APPLY $EXPR2
parseExpr2 e = do
    c <- P.try (P.satisfy (`elem` "(") >>= \x -> return $ Just x) <|> return Nothing
    parseSpaces
    case c of
        Just '(' -> do
            args <- parseApply e
            -- TODO: recursive
            return $ Just args
        Nothing  -> return Nothing

-- $EXPRS'? )
parseApply fun =
    P.try (P.char ')' >> return (AST.ExprApply fun [])) <|> firstExpr
    where
        firstExpr = do
            e <- parseExpr
            parseSpaces
            tailExprs [e]
        tailExprs exprs = do
            c <- parseIsChar ')'
            if c then
                return $ AST.ExprApply fun (reverse exprs)
            else do
                P.char ','
                parseSpaces
                e <- parseExpr
                parseSpaces
                tailExprs $ e:exprs


lexer :: T.TokenParser ()
lexer = T.makeTokenParser (haskellDef { T.reservedOpNames = ["*", "/", "+", "-"] })

parseExpr = E.buildExpressionParser table term <?> "expression"
    where
        term = do
            c <- parseIsChar '('
            if c then do
                parseSpaces
                e <- parseExpr
                parseSpaces
                P.char ')'
                return e
            else
                parseExpr0 <?> "simple expression"
        reservedOp  = T.reservedOp lexer
        binary name fun assoc = E.Infix (do{ reservedOp name; return fun }) assoc
        prefix name fun = E.Prefix (do{ reservedOp name; return fun })
        table = [[prefix "-" opNegate],
                 [binary "*" (opBin "*") E.AssocLeft,
                  binary "/" (opBin "/") E.AssocLeft ],
                 [binary "+" (opBin "+") E.AssocLeft,
                  binary "-" (opBin "-") E.AssocLeft ]]
        opNegate = AST.ExprPrefix "-"
        opBin = AST.ExprBin
