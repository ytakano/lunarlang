{-# LANGUAGE FlexibleContexts #-}

module Parser (
    parse
) where

import qualified AST
import           Control.Applicative
import           Text.Parsec         ((<?>))
import qualified Text.Parsec         as P


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

    return $ AST.Defun (AST.Fun id args ret preds [])

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

-- $PREDS := require $PREDS_
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

-- $QTYPE := $QUALIFIER? $TYPE
parseQType = do
    id <- parseID
    let q = if id == "shared" then AST.Shared else AST.Lin
    t <- if id /= "shared" && id /= "lin" then do
        parseSpaces
        parseTID id
    else do
        parseSpacesPlus
        parseType
    return $ AST.QType q t

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

-- $TYPE := $IDTVAR <$QTYPES>? | func ( $QTYPES? ) $TYPESPEC | ( $QTYPES? ) | [ $QTYPE ]
-- $IDTVAR := $CSID | $TVAR
parseType = do
    c <- P.try (P.char '(') <|> P.char '[' <|> P.char '`' <|> parseNonum
    case c of
        '(' -> parseTupleType
        '[' -> parseArrayType
        '`' -> parseTVar
        _   -> do
            t <- P.try parseID <|> return ""
            let id = c:t
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

-- $CSID = $ID | $ID : $CSID
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
-- $ARG := $ID $TYPESPEC?
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
