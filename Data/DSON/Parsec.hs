module Data.DOSN.Parsec (
    Dson(..),
    dsonTop
    ) where

import Control.Applicative
import Control.Monad
import Text.Parsec hiding (Empty, many, optional, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

data Dson = DSDict [(String, Dson)] | DSArray [Dson] | DSString String | DSNumber Double | Yes | No | Empty
    deriving (Show, Eq)

lexer = P.makeTokenParser emptyDef
symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
naturalOrFloat = P.naturalOrFloat lexer

dsonString :: Parser Dson
dsonString = fmap DSString stringLiteral

dsonInt :: Parser Dson
dsonInt = pure (DSNumber . (either fromInteger id)) <*> naturalOrFloat

dsonBool :: Parser Dson
dsonBool = try (string "yes" >> return Yes) <|>
           try (string "no" >> return No) <|>
               (string "empty" >> return Empty)

dsonValue :: Parser Dson
dsonValue = try dsonArray <|> try dsonDict <|> try dsonBool <|> try dsonInt <|> dsonString

dsonArray :: Parser Dson
dsonArray = do
    symbol "so"
    vals <- sepBy dsonValue dsonArraySep
    symbol "many"
    return $ DSArray vals
  where dsonArraySep = try (symbol "also") <|> (symbol "and")

dsonDict :: Parser Dson
dsonDict = DSDict <$> (symbol "such" *> (kvPairs `sepBy` dsonSep) <* symbol "wow")
  where kvPairs = (,) <$> stringLiteral <*> (symbol "is" *> dsonValue)
        dsonSep = oneOf ".,!?" <* spaces

dsonTop :: Parser Dson
dsonTop = try dsonDict <|> dsonArray
