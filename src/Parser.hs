{-# LANGUAGE TypeApplications #-}

module Parser where

import Data.Functor.Identity (Identity)
import Language
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec

-- type synonyms for operators from parsec
type Op a = Operator String () Identity a
type Ops a = OperatorTable String () Identity a

parseInteger :: Parser Int
parseInteger = fromInteger @Int <$> integer refLexer

parseRefI64 :: (RefSym repr) => Parser repr
parseRefI64 = i64 <$> parseInteger

parseOp :: String -> Parser ()
parseOp = reservedOp refLexer

binaryOp :: (RefSym repr) => String -> (repr -> repr -> repr) -> Assoc -> Op repr
binaryOp x f = Infix (parseOp x >> pure f)

table :: (RefSym repr) => Ops repr
table =
    [
        [ binaryOp "*" (bin Mul) AssocLeft
        ]
    ,
        [ binaryOp "+" (bin Add) AssocLeft
        , binaryOp "-" (bin Sub) AssocLeft
        ]
    ]

expr :: (RefSym repr) => Parser repr
expr = buildExpressionParser table parseRefI64

parser :: (RefSym repr) => String -> Either String repr
parser input =
    case parse expr "rc" input of
        Left err -> Left $ show err
        Right val -> Right val
