import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Language (javaStyle)
import qualified Text.ParserCombinators.Parsec.Token as P

type Dict = [(String, Dson)]

data Dson = Dict Dict | Array [Dson] | String String | Number Int | Yes | No | Empty
    deriving Show

lexer = P.makeTokenParser javaStyle
symbol = P.symbol lexer
lexeme = P.lexeme lexer

spaces = skipMany1 space

anyNotBackslashComilla :: Parser Char
anyNotBackslashComilla = noneOf "\""


dsonString :: Parser Dson
dsonString = do
     char '"'
     x <- many anyNotBackslashComilla
     char '"'
     return $ String x

dsonInt :: Parser Dson
dsonInt = liftM (Number . read) $ many1 digit

dsonBool :: Parser Dson
dsonBool = try (string "yes" >> return Yes) <|>
           try (string "no" >> return No) <|>
               (string "empty" >> return Empty)

dsonValue :: Parser Dson
dsonValue = lexeme (try dsonArray <|> try dsonDict <|> try dsonBool <|> try dsonInt <|> dsonString)

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
  where dsonDictSep = lexeme $ oneOf ".,!?"
        keyValue = do
            key <- dsonKey
            symbol "is"
            val <- dsonValue
            return (key, val)

        dsonKey = lexeme $ do
            char '"'
            v <- many (noneOf "\"")
            char '"'
            return v
