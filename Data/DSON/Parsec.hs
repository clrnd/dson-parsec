module Data.DOSN.Parsec (
    Dson(..),
    dsonTop
    ) where

import Control.Applicative
import Data.Char (isOctDigit)
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

dsonInt :: Parser Dson
dsonInt = pure (DSNumber . either fromInteger id) <*> naturalOrFloat

dsonInt2 :: Parser Dson
dsonInt2 = DSNumber <$> number
    where number = (*) <$> factor <*> float
          factor = (char '-' *> pure (-1)) <|> pure 1


octalNumber :: Parser String
octalNumber = (++) <$> (some octDigit <*> symbol "." <*> some octDigit)

octDigit :: Parser Char
octDigit = satisfy isOctDigit

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
