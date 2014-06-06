import Text.ParserCombinators.Parsec hiding (spaces)

type Dict = [(String, Dson)]
type Wut = Maybe Bool

data Dson = Dson Dict | Array [Dson] | String String | Int Int | Bool Wut
    deriving Show

stringToDsonBool :: String -> Dson
stringToDsonBool "yes" = Bool $ Just True
stringToDsonBool "no" = Bool $ Just False
stringToDsonBool "empty" = Bool Nothing

dsonString :: Parser Dson
dsonString = do
     char '"'
     x <- many (noneOf "\"")
     char '"'
     return $ String x

dsonInt :: Parser Dson
dsonInt = do
     x <- many1 digit
     return $ (Int . read) x

dsonBool :: Parser Dson
dsonBool = do
     x <- string "yes" <|> string "no" <|> string "empty"
     return $ stringToDsonBool x

dsonValue  :: Parser Dson
dsonValue = dsonBool <|> dsonInt <|> dsonString
