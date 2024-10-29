{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Parser where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Language
import Text.Parsec.Expr
import Text.Parsec.Token (commaSep, parens)
import Text.ParserCombinators.Parsec

-- helpers
whitespace :: Parser ()
whitespace =
    choice
        [ simpleWhitespace *> whitespace
        , return ()
        ]
  where
    simpleWhitespace = void $ many1 (oneOf " \t\n")

lexeme :: Parser p -> Parser p
lexeme p = p <* whitespace

commas :: Parser p -> Parser [p]
commas = commaSep refLexer

parenthesis :: Parser p -> Parser p
parenthesis = parens refLexer

parseType :: Parser Type
parseType =
    choice
        [ I64T <$ string "I64"
        , I32T <$ string "I32"
        , U64T <$ string "U64"
        , U32T <$ string "U32"
        , F64T <$ string "F64"
        , F32T <$ string "F32"
        , BoolT <$ string "Bool"
        , StringT <$ string "String"
        ]

-- type synonyms for operators from parsec
type Op a = Operator String () Identity a
type Ops a = OperatorTable String () Identity a

-- Integer 64 parsing
parseInteger :: Parser Int
parseInteger = fromInteger @Int <$> integer refLexer

parseRefI64 :: (ExprSym repr) => Parser repr
parseRefI64 = i64 <$> parseInteger

-- Integer 32 parsing
parseInteger32 :: Parser Int32
parseInteger32 = fromInteger @Int32 <$> integer refLexer

parseRefI32 :: (ExprSym repr) => Parser repr
parseRefI32 = i32 <$> parseInteger32

-- Unsigned Integer 64 parsing
parseWord64 :: Parser Word64
parseWord64 = fromInteger @Word64 <$> integer refLexer

parseRefU64 :: (ExprSym repr) => Parser repr
parseRefU64 = u64 <$> parseWord64

-- Integer 32 parsing
parseWord32 :: Parser Word32
parseWord32 = fromInteger @Word32 <$> integer refLexer

parseRefU32 :: (ExprSym repr) => Parser repr
parseRefU32 = u32 <$> parseWord32

-- Parse bools
parseRefBool :: (ExprSym repr) => Parser repr
parseRefBool = do
    r <- (reserved refLexer "true" >> pure True) <|> (reserved refLexer "false" >> pure False)
    pure $ bool r

-- parse string
parseRefStr :: (ExprSym repr) => Parser repr
parseRefStr = str <$> stringLiteral refLexer

-- parse variable
parseRefVar :: (ExprSym repr) => Parser repr
parseRefVar = var <$> identifier refLexer

-- parse any operator
parseOp :: String -> Parser ()
parseOp = reservedOp refLexer

-- parse func call
parseRefFun :: (ExprSym repr) => Parser repr
parseRefFun = do
    funName <- identifier refLexer
    fun funName <$> parenthesis (commas expr)

binaryOp :: (ExprSym repr) => String -> (repr -> repr -> repr) -> Assoc -> Op repr
binaryOp x f = Infix (parseOp x >> pure f)

table :: (ExprSym repr) => Ops repr
table =
    [
        [ binaryOp "*" (bin Mul) AssocLeft
        , binaryOp "/" (bin Div) AssocLeft
        , binaryOp "%" (bin Mod) AssocLeft
        ]
    ,
        [ binaryOp "+" (bin Add) AssocLeft
        , binaryOp "-" (bin Sub) AssocLeft
        ]
    ,
        [ binaryOp ">" (bin Gt) AssocLeft
        , binaryOp "<" (bin Lt) AssocLeft
        , binaryOp ">=" (bin GtE) AssocLeft
        , binaryOp "<=" (bin LtE) AssocLeft
        ]
    ,
        [ binaryOp "=" (bin Equal) AssocLeft
        , binaryOp "!=" (bin NEqual) AssocLeft
        ]
    , [binaryOp "/\\" (bin And) AssocLeft]
    , [binaryOp "^" (bin XOr) AssocLeft]
    , [binaryOp "\\/" (bin Or) AssocLeft]
    ]

term :: (ExprSym repr) => Parser repr
term =
    parseRefI64
        <|> parseRefI32
        <|> parseRefU64
        <|> parseRefU32
        <|> parseRefBool
        <|> parseRefStr
        <|> parseRefVar

expr :: (ExprSym repr) => Parser repr
expr = try parseRefFun <|> lexeme (buildExpressionParser table term)

-- Parse an expression statement
parseExprStm :: (StmSym repr) => Parser repr
parseExprStm = exprStm <$> lexeme (expr <* char ';')

parseFunStm :: (StmSym repr) => Parser repr
parseFunStm = do
    funName <- identifier refLexer
    args <- parenthesis (commas expr)
    _ <- char ';'
    pure $ funStm funName args

-- parse return statement
parseReturn :: Parser ()
parseReturn = reserved refLexer "RETURN" <|> reserved refLexer "Return" <|> reserved refLexer "return"

parseReturnStm :: (StmSym repr) => Parser repr
parseReturnStm = do
    parseReturn
    parseExprStm

-- Parse an assignment statement
parseAssignment :: (StmSym repr) => Parser repr
parseAssignment = do
    v <- identifier refLexer
    reservedOp refLexer ":="
    assign v <$> parseExprStm

-- Parse if statement
parseIf :: Parser ()
parseIf = reserved refLexer "IF" <|> reserved refLexer "If" <|> reserved refLexer "if"

parseThen :: Parser ()
parseThen = reserved refLexer "THEN" <|> reserved refLexer "Then" <|> reserved refLexer "then"

parseElse :: Parser ()
parseElse = reserved refLexer "ELSE" <|> reserved refLexer "Else" <|> reserved refLexer "else"

parseIfStm :: (StmSym repr) => Parser repr
parseIfStm = do
    parseIf
    e <- expr @Expr
    parseThen
    thns <- stm
    parseElse
    ifStm e thns <$> stm

-- parse do
parseDo :: Parser ()
parseDo = reserved refLexer "DO" <|> reserved refLexer "Do" <|> reserved refLexer "do"

parseOd :: Parser ()
parseOd = reserved refLexer "OD" <|> reserved refLexer "Od" <|> reserved refLexer "od"

parseDoStm :: (StmSym repr) => Parser repr
parseDoStm = do
    parseDo
    e <- expr @Expr
    reservedOp refLexer "->"
    stms <- many1 stm
    parseOd
    pure $ doStm e stms

-- function def
parseProcedure :: Parser ()
parseProcedure = reserved refLexer "PROCEDURE" <|> reserved refLexer "Procedure" <|> reserved refLexer "procedure"

parseParam :: Parser Parameter
parseParam = do
    paramName <- identifier refLexer
    paramType <- optionMaybe (reservedOp refLexer ":" >> parseType)
    pure (paramName, paramType)

parseBegin :: Parser ()
parseBegin = reserved refLexer "BEGIN" <|> reserved refLexer "Begin" <|> reserved refLexer "begin"

parseEnd :: Parser ()
parseEnd = (reserved refLexer "END" <|> reserved refLexer "End" <|> reserved refLexer "end") <* char '.'

-- parse var decl
parseVar :: Parser ()
parseVar = reserved refLexer "VAR" <|> reserved refLexer "Var" <|> reserved refLexer "var"

parseVarStm :: (StmSym repr) => Parser repr
parseVarStm = do
    parseVar
    v <- identifier refLexer <* char ':'
    whitespace
    t <- parseType <* char '.'
    pure $ varStm v t

-- parse con decl
parseCon :: Parser ()
parseCon = reserved refLexer "CON" <|> reserved refLexer "Con" <|> reserved refLexer "con"

parseConStm :: (StmSym repr) => Parser repr
parseConStm = do
    parseCon
    v <- identifier refLexer <* char ':'
    whitespace
    t <- parseType <* char '.'
    pure $ conStm v t

-- statements
stm :: (StmSym repr) => Parser repr
stm = try (lexeme parseFunStm) <|> parseDoStm <|> parseIfStm <|> parseReturnStm <|> parseAssignment <|> parseVarStm <|> parseExprStm

-- program
parseProcDecl :: (ProgSym repr) => Parser repr
parseProcDecl = do
    parseProcedure
    fname <- lexeme (identifier refLexer)
    params <- parenthesis (commas parseParam)
    mvarDecl <- lexeme $ optionMaybe parseVarStm
    parseBegin
    stms <- lexeme $ many1 stm
    parseEnd
    pure $ proc fname params mvarDecl stms

prog :: (ProgSym repr) => Parser repr
prog = program <$> many1 (lexeme parseProcDecl)

parser :: (ProgSym repr) => String -> Either String repr
parser input =
    case parse prog "rc" input of
        Left err -> Left $ show err
        Right val -> Right val
