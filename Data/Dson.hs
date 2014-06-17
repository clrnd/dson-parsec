module Data.Dson where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Dson.Parsec
import Data.Dson.Serializer (dsonToJson)

run :: Parser Dson -> String -> IO ()
run p input = case (parse p "" input) of
                  Left err -> putStr "parse error at " >> print err
                  Right x  -> print $ dsonToJson x

main :: IO ()
main = run dsonTop =<< getLine
