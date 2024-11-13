module Language (
    refLexer,
    reserved,
    reservedOp,
    integer,
    stringLiteral,
    identifier,
    float,
    Type (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Void (Void)
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

data Type
    = NatT
    | BoolT
    | IntT
    | RealT
    | CharT
    | TextT
    | UnitT
    deriving (Eq)

data TypeExpr

data ValueBinOp
    = Equal
    | NotEqual
    | And
    | Or
    | Impl

data ValueExpr
    = BoolVE Bool
    | ChaosVE Void
    | If ValueExpr ValueExpr (Maybe [ValueExpr]) ValueExpr
    | BinOpVE ValueBinOp ValueExpr ValueExpr
    | NotVE ValueExpr

data TypeDeclaration
    = Sort Text
    | AbstractType Text TypeExpr

data ValueDeclaration = ValueDeclaration
    { valueIdentifier :: Text
    , valueTypeExpr :: TypeExpr
    }

data AxiomDeclaration = AxiomDeclaration
    { axiomNaming :: Maybe Text
    , axiomValueExpr :: NonEmpty ValueExpr
    }
data Declaration
    = TypeDecl TypeDeclaration
    | ValueDecl ValueDeclaration
    | AxiomDecl AxiomDeclaration

data Module = Module
    { moduleName :: Text
    , declarations :: [Declaration]
    }

rslLexer :: TokenParser st
rslLexer = makeTokenParser refCalcDef

rslDef :: LanguageDef st
rslDef =
    LanguageDef
        { commentStart = "/*"
        , commentEnd = "*/"
        , commentLine = undefined
        , nestedComments = False
        , identStart = letter
        , identLetter = alphaNum <|> char '_'
        , opStart = opLetter emptyDef
        , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , reservedOpNames = refCalcOps
        , reservedNames = refCalcKeywords
        , caseSensitive = True
        }

rslTypeOps :: [Text]
rslTypeOps = ["-set", "><", "->"]

rslOps :: [Text]
rslOps = ["=", "~=", "is", "isin", "=>", ":-", "union", "card"]

rslKeywords :: [Text]
rslKeywords =
    [ "class"
    , "type"
    , "value"
    , "axiom"
    , "end"
    , "Nat"
    , "Bool"
    , "Int"
    , "Real"
    , "Char"
    , "Text"
    , "Unit"
    , "true"
    , "false"
    , "chaos"
    , "extend"
    , "with"
    , "if"
    , "then"
    , "else"
    , "elsif"
    ]
