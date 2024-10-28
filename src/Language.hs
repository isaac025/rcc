module Language (
    refLexer,
    ExprSym (..),
    StmSym (..),
    reserved,
    reservedOp,
    integer,
    stringLiteral,
    identifier,
    float,
    Expr (..),
    Stm (..),
    BinOp (..),
    Int32,
    Word64,
    Word32,
) where

import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import Text.Parsec.Token (
    TokenParser,
    float,
    identifier,
    integer,
    makeTokenParser,
    reserved,
    reservedOp,
    stringLiteral,
 )
import Text.ParserCombinators.Parsec.Language

-- AST & BinOp
data Expr
    = I64 Int
    | I32 Int32
    | F64 Double
    | F32 Float
    | U64 Word64
    | U32 Word32
    | Boolean Bool
    | Str String
    | BinE BinOp Expr Expr
    deriving (Show)

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

-- Language Expression Symantics
class ExprSym repr where
    i64 :: Int -> repr
    i32 :: Int32 -> repr
    u64 :: Word64 -> repr
    u32 :: Word32 -> repr
    f64 :: Double -> repr
    f32 :: Float -> repr
    str :: String -> repr
    bool :: Bool -> repr
    bin :: BinOp -> repr -> repr -> repr

instance ExprSym Expr where
    i64 = I64
    i32 = I32
    f64 = F64
    f32 = F32
    bool = Boolean
    u64 = U64
    u32 = U32
    bin = BinE
    str = Str

-- Language Statement Symantics
data Stm
    = ExprStm Expr
    | IfStm Expr Stm Stm
    | Return Stm
    deriving (Show)

class StmSym repr where
    exprStm :: Expr -> repr
    ifStm :: Expr -> repr -> repr -> repr
    returnStm :: repr -> repr

instance StmSym Stm where
    ifStm = IfStm
    returnStm = Return
    exprStm = ExprStm

-- Lexer
refLexer :: TokenParser st
refLexer = makeTokenParser refCalcDef

refCalcDef :: LanguageDef st
refCalcDef =
    LanguageDef
        { commentStart = "{"
        , commentEnd = "}"
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
