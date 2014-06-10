import Control.Applicative
import Control.Monad
import Text.Parsec hiding (Empty, many, optional, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as P

type Dict = [(String, Dson)]

data Dson = Dict Dict | Array [Dson] | String String | Number Double | Yes | No | Empty
    deriving (Show, Eq)

lexer = P.makeTokenParser javaStyle
symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
naturalOrFloat = P.naturalOrFloat lexer

anyNotBackslashComilla :: Parser Char
anyNotBackslashComilla = noneOf "\""


dsonString :: Parser Dson
dsonString = fmap String stringLiteral

dsonInt :: Parser Dson
dsonInt = liftM (either (Number . fromInteger) Number) naturalOrFloat

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
    return $ Array vals
  where dsonArraySep = try (symbol "also") <|> (symbol "and")

dsonDict :: Parser Dson
dsonDict = do
    symbol "such"
    vals <- keyValue `sepBy` dsonDictSep
    symbol "wow"
    return $ Dict vals
  where dsonDictSep = oneOf ".,!?"
        dsonKey = stringLiteral
        keyValue = do
            key <- dsonKey
            symbol "is"
            val <- dsonValue
            return (key, val)

dsonDIct2 :: Parser [(String, Dson)]
dsonDIct2 = symbol "such" *> (kvPairs `sepBy` dsonSep) <* symbol "wow"
  where kvPairs = (,) <$> stringLiteral <*> (symbol "is" *> dsonValue)
        dsonSep = spaces *> oneOf ".,!?" <* spaces
