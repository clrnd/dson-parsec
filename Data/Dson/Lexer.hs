{-|
Module      : Data.Dson.Lexer
Copyright   : (c) Ezequiel Alvarez 2014
License     : MIT
Maintainer  : welcometothechango@gmail.com
Stability   : provisional
Portability : portable

Some lexers for the rest of the library.
-}

module Data.Dson.Lexer (symbol, stringLiteral) where

import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

lexer = P.makeTokenParser emptyDef
symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
