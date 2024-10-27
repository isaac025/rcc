module Language (
    refLexer,
    RefSym (..),
    reserved,
    reservedOp,
    integer,
    identifier,
    float,
    Expr (..),
    BinOp (..),
) where

import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import Text.Parsec.Token (TokenParser, float, identifier, integer, makeTokenParser, reserved, reservedOp)
import Text.ParserCombinators.Parsec.Language

-- AST & BinOp
data Expr
    = I64 Int
    | I32 Int32
    | U64 Word64
    | U32 Word32
    | Boolean Bool
    | BinE BinOp Expr Expr

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    | XOr

-- Language Symantics
class RefSym repr where
    i64 :: Int -> repr
    i32 :: Int32 -> repr
    u64 :: Word64 -> repr
    u32 :: Word32 -> repr
    bool :: Bool -> repr
    bin :: BinOp -> repr -> repr -> repr

instance RefSym Expr where
    i64 = I64
    i32 = I32
    bool = Boolean
    u64 = U64
    u32 = U32
    bin = BinE

-- Lexer
refLexer :: TokenParser st
refLexer = makeTokenParser refCalcDef

refCalcDef :: LanguageDef st
refCalcDef =
    LanguageDef
        { commentStart = "/*"
        , commentEnd = "*/"
        , commentLine = "//"
        , nestedComments = True
        , identStart = letter
        , identLetter = alphaNum <|> char '_'
        , opStart = opLetter emptyDef
        , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , reservedOpNames = refCalcOps
        , reservedNames = refCalcKeywords
        , caseSensitive = True
        }

refCalcOps :: [String]
refCalcOps =
    [ "->"
    , ":="
    , "."
    , ";"
    , "+"
    , "-"
    , "*"
    , "/"
    , "\\/"
    , "/\\"
    , "^"
    ]

refCalcKeywords :: [String]
refCalcKeywords =
    [ "PROCEDURE"
    , "Procedure"
    , "procedure"
    , "I64"
    , "I32"
    , "U64"
    , "U32"
    , "F64"
    , "F32"
    , "Bool"
    , "true"
    , "false"
    , "VAR"
    , "Var"
    , "var"
    , "CON"
    , "Con"
    , "con"
    , "BEGIN"
    , "Begin"
    , "begin"
    , "END"
    , "End"
    , "end"
    , "IF"
    , "If"
    , "if"
    , "THEN"
    , "Then"
    , "then"
    , "ELSE"
    , "Else"
    , "else"
    , "DO"
    , "Do"
    , "do"
    , "OD"
    , "Od"
    , "od"
    , "RETURN"
    , "Return"
    , "return"
    ]
