module Parser where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text, pack)
import Expressions
import Language
import Text.Parsec (
    ParseError,
    choice,
    many1,
    oneOf,
    optionMaybe,
    parse,
    parserFail,
    sepBy1,
    try,
    (<|>),
 )
import Text.Parsec.Expr (Operator (..))
import Text.Parsec.Text.Lazy (Parser)
import Text.Parsec.Token (
    GenTokenParser (..),
    parens,
 )

rslIdentifier :: Parser Text
rslIdentifier = pack <$> rslLexeme (identifier rslLexer)

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
rslAxiomDef = undefined

rslAxiomDeclarations :: Parser [AxiomDeclaration]
rslAxiomDeclarations = undefined

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
    -- maxioms <- optionMaybe $ AxiomDecl <$> rslAxiomDeclarations
    let mdecls = catMaybes [mtypes, mvalues]
    case nonEmpty mdecls of
        Nothing -> parserFail "empty declarations, expected at least value declarations"
        Just decls -> do
            rslReserved "end"
            pure $ Module mname decls mextension

parser :: Text -> Either ParseError Module
parser = parse parseModule "rdm"
