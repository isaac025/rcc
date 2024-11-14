module Expressions where

import Data.List.NonEmpty (NonEmpty)
import Data.Text.Lazy (Text)
import Data.Void (Void)
import Numeric.Natural

data Module = Module
    { moduleName :: Text
    , declarations :: NonEmpty Declaration
    , extension :: Maybe Text
    }
    deriving (Show)

data Declaration
    = TypeDecl [TypeDeclaration]
    | ValueDecl [ValueDeclaration]
    | AxiomDecl [AxiomDeclaration]
    deriving (Show)

data TypeDeclaration
    = Sort Text
    | AbstractType Text TypeExpr
    deriving (Show)

data Type
    = NatT
    | BoolT
    | IntT
    | RealT
    | CharT
    | TextT
    | UnitT
    | AdtT Text
    deriving (Show, Eq)

data TypeExpr
    = TypeTE Type
    | SetTE Type
    | ProductTE [TypeExpr]
    | FuncTE TypeExpr TypeExpr
    | AppTE Text [ValueExpr] Text ValueExpr ValueExpr
    deriving (Show)

data ValueDeclaration = ValueDeclaration
    { valueIdentifier :: Text
    , valueTypeExpr :: TypeExpr
    }
    deriving (Show)

type TypingList = NonEmpty ValueDeclaration -- typing same as a value declaration

data AxiomDeclaration = AxiomDeclaration
    { axiomNaming :: Maybe Text
    , axiomValueExpr :: NonEmpty ValueExpr
    }
    deriving (Show)

data ValueExpr
    = BoolVE Bool
    | If ValueExpr ValueExpr (Maybe [ValueExpr]) ValueExpr
    | NotVE ValueExpr
    | BinOpVE ValueBinOp ValueExpr ValueExpr
    | ChaosVE Void
    | QuantVE Quantifier TypingList ValueExpr
    | IntVE Int
    | Abs ValueExpr
    | NatVE Natural
    | RealVE Double
    | CharVE Char
    | TextVE Text
    | UnitVE ()
    | ProductVE (NonEmpty ValueExpr)
    | AppVE Text [ValueExpr]
    | Pre ValueExpr
    | FuncVE TypingList ValueExpr
    | Post ValueExpr
    deriving (Show)

data ValueBinOp
    = Equal
    | NotEqual
    | Is
    | IsIn
    | Union
    | And
    | Or
    | Impl
    | Gt
    | GtEq
    | Lt
    | LtEq
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Exp
    deriving (Show)

data Quantifier = Forall | Exists | ExistsOne
    deriving (Show)
