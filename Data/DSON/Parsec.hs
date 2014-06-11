module Data.DOSN.Parsec (
    Dson(..),
    dsonTop
    ) where

import Control.Applicative
import Data.Char (isOctDigit)
import Data.Dson.Octal (octalToDouble)
import Text.Parsec (satisfy, sepBy, try, oneOf, char, spaces, parseTest)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

data Dson = DSDict [(String, Dson)] | DSArray [Dson] | DSString String | DSNumber Double | Yes | No | Empty
    deriving (Show, Eq)

lexer = P.makeTokenParser emptyDef
symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
naturalOrFloat = P.naturalOrFloat lexer
float = P.float lexer

dsonString :: Parser Dson
dsonString = DSString <$> stringLiteral

factor :: Parser Double
factor = (char '-' *> pure (-1)) <|> (char '+' *> pure 1) <|> pure 1

dsonInt :: Parser Dson
dsonInt = DSNumber <$> number
    where number = (*) <$> factor <*> octal

octal = octalToDouble <$> octalParts

octalParts :: Parser (String, Maybe String, Maybe (Double, String))
octalParts = (,,) <$> some octDigit <*> optional octalDecimals <*> optional octalExponent
    where octDigit = satisfy isOctDigit
          octalDecimals = symbol "." *> some octDigit
          octalExponent = (,) <$> (veryVERY *> factor) <*> some octDigit
          veryVERY = symbol "very" <|> symbol "VERY"

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
