import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (javaStyle)
import qualified Text.ParserCombinators.Parsec.Token as P

type Dict = [(String, Dson)]

data Dson = Dict Dict | Array [Dson] | String String | Number Double | Yes | No | Empty
    deriving Show

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
    vals <- sepBy keyValue dsonDictSep
    symbol "wow"
    return $ Dict vals
  where dsonDictSep = oneOf ".,!?"
        keyValue = do
            key <- dsonKey
            symbol "is"
            val <- dsonValue
            return (key, val)

        dsonKey = stringLiteral
