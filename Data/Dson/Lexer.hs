module Data.Dson.Lexer (symbol, stringLiteral) where

import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

lexer = P.makeTokenParser emptyDef
symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
