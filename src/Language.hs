module Language (
    rslLexer,
) where

import Control.Monad.Identity (Identity)
import Data.Text.Lazy (Text)
import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import Text.Parsec.Token (
    GenLanguageDef (..),
    GenTokenParser (..),
    makeTokenParser,
 )

rslDef :: GenLanguageDef Text st Identity
rslDef =
    LanguageDef
        { commentStart = "/*"
        , commentEnd = "*/"
        , commentLine = "//"
        , nestedComments = False
        , identStart = letter
        , identLetter = alphaNum <|> char '_'
        , opStart = opLetter rslDef
        , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , reservedOpNames = rslOps <> rslTypeOps
        , reservedNames = rslKeywords
        , caseSensitive = True
        }

rslTypeOps :: [String]
rslTypeOps = ["-set", "><", "->", "-~->", ":"]

rslOps :: [String]
rslOps =
    [ "="
    , "~="
    , "is"
    , "isin"
    , "=>"
    , ":-"
    , "union"
    , "card"
    , "~"
    , "/\\"
    , "\\/"
    , "abs"
    , "forall"
    , "exists"
    , "int"
    , "real"
    , "-\\"
    , ">="
    , ">"
    , "<="
    , "<"
    , "+"
    , "-"
    , "*"
    , "/"
    , "\\"
    , "**"
    ]

rslKeywords :: [String]
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
    , "pre"
    , "post"
    ]

rslLexer :: GenTokenParser Text st Identity
rslLexer = makeTokenParser rslDef
