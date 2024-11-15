{-# LANGUAGE TypeApplications #-}

module Parser where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Text.Lazy (Text, pack)
import Data.Void (Void)
import Expressions
import Language
import Numeric.Natural (Natural)
import Text.Parsec (
    ParseError,
    between,
    char,
    choice,
    many,
    many1,
    oneOf,
    optionMaybe,
    parse,
    parserFail,
    sepBy1,
    string,
    try,
    (<|>),
 )
import Text.Parsec.Expr (Assoc (..), Operator (..), OperatorTable, buildExpressionParser)
import Text.Parsec.Text.Lazy (Parser)
import Text.Parsec.Token (
    GenTokenParser (..),
    parens,
 )

rslIdentifier :: Parser Text
rslIdentifier = pack <$> rslLexeme (identifier rslLexer)

rslInt :: Parser Int
rslInt = fromInteger <$> integer rslLexer

rslNat :: Parser Natural
rslNat = fromInteger <$> natural rslLexer

rslReal :: Parser Double
rslReal = float rslLexer

rslChar :: Parser Char
rslChar = charLiteral rslLexer

rslText :: Parser Text
rslText = pack <$> stringLiteral rslLexer

rslParens :: Parser a -> Parser a
rslParens = parens rslLexer

rslReserved :: String -> Parser ()
rslReserved = reserved rslLexer

rslSemiSep :: Parser a -> Parser [a]
rslSemiSep = semiSep rslLexer

rslCommaSep :: Parser a -> Parser [a]
rslCommaSep = commaSep rslLexer

rslReservedOp :: String -> Parser ()
rslReservedOp = reservedOp rslLexer

rslPrefixOp :: String -> (a -> a) -> Operator Text () Identity a
rslPrefixOp s f = Prefix (rslReservedOp s >> pure f)

rslLexeme :: Parser p -> Parser p
rslLexeme p = p <* whitespace

whitespace :: Parser ()
whitespace =
    choice
        [ simpleWhitespace *> whitespace
        , pure ()
        ]
  where
    simpleWhitespace = void $ many1 (oneOf " \t\n")

-- type expressions
rslType :: Parser Type
rslType =
    (NatT <$ rslReserved "Nat")
        <|> (BoolT <$ rslReserved "Bool")
        <|> (IntT <$ rslReserved "Int")
        <|> (RealT <$ rslReserved "Real")
        <|> (CharT <$ rslReserved "Char")
        <|> (TextT <$ rslReserved "Text")
        <|> (UnitT <$ rslReserved "Unit")
        <|> (AdtT <$> rslIdentifier)

rslTypingExpr :: Parser TypeExpr
rslTypingExpr = TypeTE <$> rslType

rslTypeSetExpr :: Parser TypeExpr
rslTypeSetExpr = do
    rtype <- rslType
    rslReservedOp "-set"
    pure $ SetTE rtype

rslProductExpr :: Parser TypeExpr
rslProductExpr = do
    rslTypes <- rslTypingExpr `sepBy1` rslReservedOp "><"
    if length rslTypes == 1 then parserFail ">< expects from and to types" else pure $ ProductTE rslTypes

rslSimpleTypeExpr :: Parser TypeExpr
rslSimpleTypeExpr = try rslTypeSetExpr <|> rslTypingExpr

rslFunctionTypeExpr :: Parser TypeExpr
rslFunctionTypeExpr = do
    fromExpr <- try rslProductExpr <|> rslSimpleTypeExpr
    rslReservedOp "-~->" <|> rslReservedOp "->"
    FuncTE fromExpr <$> (try rslProductExpr <|> rslSimpleTypeExpr)

rslTypeExpr :: Parser TypeExpr
rslTypeExpr = try rslFunctionTypeExpr <|> rslSimpleTypeExpr

-- value expressions
rslBoolValueExpr :: Parser ValueExpr
rslBoolValueExpr = do
    boolVal <- rslLexeme (True <$ rslReserved "true") <|> (False <$ rslReserved "false")
    pure $ BoolVE boolVal

rslIntValueExpr :: Parser ValueExpr
rslIntValueExpr = IntVE <$> rslLexeme rslInt

rslNatValueExpr :: Parser ValueExpr
rslNatValueExpr = NatVE <$> rslLexeme rslNat

rslRealValueExpr :: Parser ValueExpr
rslRealValueExpr = RealVE <$> rslLexeme rslReal

rslCharValueExpr :: Parser ValueExpr
rslCharValueExpr = CharVE <$> rslLexeme rslChar

rslTextValueExpr :: Parser ValueExpr
rslTextValueExpr = TextVE <$> rslLexeme rslText

rslUnitValueExpr :: Parser ValueExpr
rslUnitValueExpr = UnitVE <$> void (rslLexeme $ string "()")

rslChaosValueExpr :: Parser ValueExpr
rslChaosValueExpr = ChaosVE <$> ((Proxy @Void) <$ rslReserved "chaos")

rslIdValueExpr :: Parser ValueExpr
rslIdValueExpr = IdVE <$> rslLexeme rslIdentifier

rslEmptySetValueExpr :: Parser ValueExpr
rslEmptySetValueExpr = SetVE <$ rslLexeme (string "{}")

rslTypingList :: Parser TypingList
rslTypingList = do
    rslTypings <- rslCommaSep rslValueDef
    case nonEmpty rslTypings of
        Nothing -> parserFail "parsing error on typing"
        Just typings -> pure typings

rslBinaryOp :: String -> (a -> a -> a) -> Assoc -> Operator Text () Identity a
rslBinaryOp txt f = Infix (rslReservedOp txt >> pure f)

rslUnaryOp :: String -> (a -> a) -> Operator Text () Identity a
rslUnaryOp txt f = Prefix (rslReservedOp txt >> pure f)

table :: OperatorTable Text () Identity ValueExpr
table =
    [
        [ rslUnaryOp "~" (UnaryOpVE Not)
        , rslUnaryOp "abs" (UnaryOpVE Abs)
        , rslUnaryOp "int" (UnaryOpVE IntC)
        , rslUnaryOp "real" (UnaryOpVE RealC)
        , rslUnaryOp "card" (UnaryOpVE Card)
        , rslUnaryOp "len" (UnaryOpVE Len)
        , rslUnaryOp "inds" (UnaryOpVE Inds)
        , rslUnaryOp "elems" (UnaryOpVE Elems)
        , rslUnaryOp "hd" (UnaryOpVE Hd)
        , rslUnaryOp "tl" (UnaryOpVE Tl)
        , rslUnaryOp "dom" (UnaryOpVE Dom)
        , rslUnaryOp "rng" (UnaryOpVE Rng)
        ]
    ,
        [ rslBinaryOp ":" (BinOpVE Func) AssocNone
        ]
    ,
        [ rslBinaryOp "**" (BinOpVE Exp) AssocNone
        ]
    ,
        [ rslBinaryOp "*" (BinOpVE Mul) AssocLeft
        , rslBinaryOp "/" (BinOpVE Div) AssocLeft
        ]
    ,
        [ rslBinaryOp "+" (BinOpVE Add) AssocLeft
        , rslBinaryOp "-" (BinOpVE Sub) AssocLeft
        , rslBinaryOp "\\" (BinOpVE Rem) AssocLeft
        , rslBinaryOp "union" (BinOpVE Union) AssocLeft
        ]
    ,
        [ rslBinaryOp "=" (BinOpVE Equal) AssocNone
        , rslBinaryOp "=" (BinOpVE NotEqual) AssocNone
        , rslBinaryOp ">" (BinOpVE Gt) AssocNone
        , rslBinaryOp "<" (BinOpVE Lt) AssocNone
        , rslBinaryOp ">=" (BinOpVE GtEq) AssocNone
        , rslBinaryOp "<=" (BinOpVE LtEq) AssocNone
        , rslBinaryOp "isin" (BinOpVE IsIn) AssocNone
        ]
    , [rslBinaryOp "/\\" (BinOpVE And) AssocRight]
    , [rslBinaryOp "\\/" (BinOpVE Or) AssocRight]
    , [rslBinaryOp "=>" (BinOpVE Impl) AssocRight]
    ,
        [ rslBinaryOp "is" (BinOpVE Is) AssocNone
        , rslUnaryOp "post" (UnaryOpVE Post)
        ]
    ]

valueExprTerms :: Parser ValueExpr
valueExprTerms = rslIdValueExpr <|> rslEmptySetValueExpr

rslSimpleValueExpr :: Parser ValueExpr
rslSimpleValueExpr = buildExpressionParser table valueExprTerms

rslQuantValueExpr :: Parser ValueExpr
rslQuantValueExpr = do
    quant <- (Forall <$ rslReservedOp "forall") <|> (Exists <$ rslReservedOp "exists")
    typings <- rslTypingList
    rslReservedOp ":-"
    QuantVE quant typings <$> valueExprTerms

rslFuncAppValueExpr :: Parser ValueExpr
rslFuncAppValueExpr = do
    funId <- rslIdentifier
    AppVE funId <$> between (char '(') (char ')') (rslCommaSep valueExprTerms)

valueExpr :: Parser ValueExpr
valueExpr = rslFuncAppValueExpr

rslIfValueExpr :: Parser ValueExpr
rslIfValueExpr = do
    rslReserved "if"
    cnd <- rslSimpleValueExpr
    rslReserved "then"
    thn <- rslSimpleValueExpr
    elsifs <- optionMaybe $ many rslElsifExpr
    rslReserved "else"
    If cnd thn elsifs <$> rslSimpleValueExpr
  where
    rslElsifExpr :: Parser (ValueExpr, ValueExpr)
    rslElsifExpr = do
        rslReserved "elsif"
        c <- rslSimpleValueExpr
        rslReserved "then"
        expr <- rslSimpleValueExpr
        pure (c, expr)

-- declarations
rslTypeDef :: Parser TypeDeclaration
rslTypeDef =
    try (AbstractType <$> rslIdentifier <* rslReservedOp "=" <*> rslTypeSetExpr)
        <|> (Sort <$> rslIdentifier)

rslTypeDeclarations :: Parser [TypeDeclaration]
rslTypeDeclarations = do
    rslReserved "type"
    rslCommaSep rslTypeDef

rslValueDef :: Parser ValueDeclaration
rslValueDef = do
    valueId <- rslIdentifier
    rslReservedOp ":"
    ValueDeclaration valueId <$> rslTypeExpr

rslValueDeclarations :: Parser [ValueDeclaration]
rslValueDeclarations = do
    rslReserved "value"
    rslCommaSep rslValueDef

rslAxiomDef :: Parser AxiomDeclaration
rslAxiomDef = do
    anaming <- optionMaybe $ rslLexeme rslAxiomNaming
    AxiomDeclaration anaming <$> rslSimpleValueExpr
  where
    rslAxiomNaming :: Parser Text
    rslAxiomNaming = char '[' *> rslIdentifier <* char ']'

rslAxiomDeclarations :: Parser [AxiomDeclaration]
rslAxiomDeclarations = do
    rslReserved "axiom"
    rslCommaSep rslAxiomDef

-- module
parseReservedModType :: Parser Text
parseReservedModType = do
    rslReserved "extend"
    mextension <- rslIdentifier
    rslReserved "with"
    pure mextension

parseModule :: Parser Module
parseModule = do
    mname <- rslIdentifier
    rslReservedOp "="
    mextension <- optionMaybe parseReservedModType
    rslReserved "class"
    mtypes <- optionMaybe $ TypeDecl <$> rslTypeDeclarations
    mvalues <- optionMaybe $ ValueDecl <$> rslValueDeclarations
    maxioms <- optionMaybe $ AxiomDecl <$> rslAxiomDeclarations
    let mdecls = catMaybes [mtypes, mvalues, maxioms]
    case nonEmpty mdecls of
        Nothing -> parserFail "empty declarations, expected at least value declarations"
        Just decls -> do
            rslReserved "end"
            pure $ Module mname decls mextension

parser :: Text -> Either ParseError Module
parser = parse parseModule "rdm"