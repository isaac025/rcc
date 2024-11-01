module Language (
    refLexer,
    ExprSym (..),
    StmSym (..),
    ProgSym (..),
    ProcSym (..),
    Value (..),
    reserved,
    reservedOp,
    integer,
    stringLiteral,
    identifier,
    float,
    Expr (..),
    Stm (..),
    BinOp (..),
    Procedure (..),
    Program (..),
    Int32,
    Word64,
    Word32,
    Type (..),
    Parameter,
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
    | Var String
    | Fun String [Expr]
    | BinE BinOp Expr Expr
    deriving (Show)

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | XOr
    | Gt
    | GtE
    | Lt
    | LtE
    | Equal
    | NEqual

instance Show BinOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show And = "/\\"
    show Or = "\\/"
    show XOr = "^"
    show Gt = ">"
    show GtE = ">="
    show Lt = "<"
    show LtE = "<="
    show Equal = "="
    show NEqual = "!="

data Value
    = I64V Int
    | I32V Int32
    | F64V Double
    | F32V Float
    | U64V Word64
    | U32V Word32
    | BoolV Bool
    | StrV String
    | VarV String
    | FunV String [Expr]
    | BinV BinOp Expr Expr
    | None

instance Show Value where
    show (I64V i) = show i
    show (I32V i) = show i
    show (F64V i) = show i
    show (F32V i) = show i
    show (U64V i) = show i
    show (U32V i) = show i
    show (BoolV i) = show i
    show (StrV i) = i
    show (VarV i) = i
    show (FunV i es) = i ++ " " ++ show es
    show (BinV b e1 e2) = show e1 ++ show b ++ show e2
    show None = show ()

-- Language Expression Symantics
class ExprSym repr where
    i64 :: Int -> repr
    i32 :: Int32 -> repr
    u64 :: Word64 -> repr
    u32 :: Word32 -> repr
    f64 :: Double -> repr
    f32 :: Float -> repr
    str :: String -> repr
    var :: String -> repr
    bool :: Bool -> repr
    bin :: BinOp -> repr -> repr -> repr
    fun :: String -> [repr] -> repr

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
    var = Var
    fun = Fun

-- Language Statement Symantics
data Type
    = I64T
    | I32T
    | U64T
    | U32T
    | F64T
    | F32T
    | BoolT
    | StringT

instance Show Type where
    show I64T = "I64"
    show I32T = "I32"
    show U64T = "U64"
    show U32T = "I32"
    show F64T = "F64"
    show F32T = "F32"
    show BoolT = "Boolean"
    show StringT = "String"

type Parameter = (String, Maybe Type)

data Stm
    = ExprStm Expr
    | Return Expr
    | FunStm String [Expr]
    | VarStm String Type
    | AssignStm String Stm
    | If Expr Stm Stm
    | Do Expr [Stm]
    deriving (Show)

class StmSym repr where
    exprStm :: Expr -> repr
    ret :: Expr -> repr
    assign :: String -> repr -> repr
    ifStm :: Expr -> repr -> repr -> repr
    varStm :: String -> Type -> repr
    funStm :: String -> [Expr] -> repr
    doStm :: Expr -> [repr] -> repr

instance StmSym Stm where
    exprStm = ExprStm
    assign = AssignStm
    ifStm = If
    varStm = VarStm
    ret = Return
    funStm = FunStm
    doStm = Do

data Procedure = Procedure String [Parameter] (Maybe Stm) [Stm]
    deriving (Show)

class ProcSym repr where
    procedure :: String -> [Parameter] -> Maybe Stm -> [Stm] -> repr

instance ProcSym Procedure where
    procedure = Procedure

data Program = Program [Procedure]
    deriving (Show)

class ProgSym repr where
    program :: [Procedure] -> repr

instance ProgSym Program where
    program = Program

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
