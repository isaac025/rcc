module Language (
    refLexer,
    RefSym (..),
    reserved,
    reservedOp,
    integer,
    identifier,
    float,
    Expr (..),
    Type (..),
    BinOp (..),
    Int32,
    Word64,
    Word32,
) where

import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import Text.Parsec.Token (TokenParser, float, identifier, integer, makeTokenParser, reserved, reservedOp)
import Text.ParserCombinators.Parsec.Language

-- AST & BinOp & Types
data Type
    = I64T
    | I32T
    | F64T
    | F32T
    | U64T
    | U32T
    | BoolT

data Expr
    = I64 Int
    | I32 Int32
    | F64 Double
    | F32 Float
    | U64 Word64
    | U32 Word32
    | Boolean Bool
    | Var String Type
    | Con String Type
    | BinE BinOp Expr Expr

instance Show Expr where
    show (I64 x) = show x ++ " : I64"
    show (I32 x) = show x ++ " : I32"
    show (F64 x) = show x ++ " : F64"
    show (F32 x) = show x ++ " : F32"
    show (U64 x) = show x ++ " : U64"
    show (U32 x) = show x ++ " : U32"
    show (Var x _) = x
    show (Con x _) = x
    show (Boolean x) = show x ++ " : Bool"
    show (BinE o e1 e2) = show e1 ++ " " ++ show o ++ " " ++ show e2

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    | XOr

instance Show BinOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show And = "/\\"
    show Or = "\\/"
    show XOr = "^"

-- Language Symantics
class RefSym repr where
    i64 :: Int -> repr
    i32 :: Int32 -> repr
    u64 :: Word64 -> repr
    u32 :: Word32 -> repr
    f64 :: Double -> repr
    f32 :: Double -> repr
    bool :: Bool -> repr
    bin :: BinOp -> repr -> repr -> repr

instance RefSym Expr where
    i64 = I64
    i32 = I32
    f64 = F64
    f32 = F32
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
        { commentStart = "(*"
        , commentEnd = "*)"
        , commentLine = "?"
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
