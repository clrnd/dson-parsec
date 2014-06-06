import Text.ParserCombinators.Parsec hiding (spaces)

type Dict = [(String, Dson)]
type Wut = Maybe Bool

data Dson = Dson Dict | Array [Dson] | String String | Int Int | Bool Wut
    deriving Show

dsonFile :: Parser Dson
dsonFile = do
    string "such"
    char '"'
    key <- many (noneOf "\"")
    char '"'
    string "wow"
    return $ Dson [(key, Int 6)]

dsonString :: Parse Dson
dsonString = do
     char '"'
     x <- many (noneOf "\"")
     char '"'
     return $ String x
