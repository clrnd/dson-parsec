module Data.Dson.Parsec (
    Dson(..),
    dsonTop
    ) where

import Data.Dson.Octal
import Data.Dson.Lexer

import Control.Applicative
import Text.Parsec (sepBy, try, oneOf, spaces, parseTest)
import Text.Parsec.String (Parser)

data Dson = DSDict [(String, Dson)] | DSArray [Dson] | DSString String | DSNumber Double | Yes | No | Empty
    deriving (Show, Eq)

dsonString :: Parser Dson
dsonString = DSString <$> stringLiteral

dsonInt :: Parser Dson
dsonInt = DSNumber <$> octal <* spaces

dsonBool :: Parser Dson
dsonBool =     (symbol "yes"   *> pure Yes)
           <|> (symbol "no"    *> pure No)
           <|> (symbol "empty" *> pure Empty)

dsonValue :: Parser Dson
dsonValue = try dsonArray <|> try dsonDict <|> try dsonBool <|> try dsonInt <|> dsonString

dsonArray = DSArray <$> (symbol "so" *> dsonValue `sepBy` arraySep <* symbol "many")
    where arraySep = try (symbol "also") <|> symbol "and"

dsonDict :: Parser Dson
dsonDict = DSDict <$> (symbol "such" *> (kvPairs `sepBy` dictSep) <* symbol "wow")
    where kvPairs = (,) <$> stringLiteral <*> (symbol "is" *> dsonValue)
          dictSep = oneOf ".,!?" <* spaces

dsonTop :: Parser Dson
dsonTop = try dsonDict <|> dsonArray
