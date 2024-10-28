{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Parser where

import Data.Functor.Identity (Identity)
import Language
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec

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

-- parse any operator
parseOp :: String -> Parser ()
parseOp = reservedOp refLexer

binaryOp :: (ExprSym repr) => String -> (repr -> repr -> repr) -> Assoc -> Op repr
binaryOp x f = Infix (parseOp x >> pure f)

table :: (ExprSym repr) => Ops repr
table =
    [
        [ binaryOp "*" (bin Mul) AssocLeft
        , binaryOp "/" (bin Div) AssocLeft
        ]
    ,
        [ binaryOp "+" (bin Add) AssocLeft
        , binaryOp "-" (bin Sub) AssocLeft
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

expr :: (ExprSym repr) => Parser repr
expr = buildExpressionParser table term

stm :: (ExprSym repr, StmSym repr) => Parser repr
stm = exprStm <$> expr <* char ';'

parser :: (ExprSym repr, StmSym repr) => String -> Either String repr
parser input =
    case parse stm "rc" input of
        Left err -> Left $ show err
        Right val -> Right val
