module Data.Dson where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Dson.Parsec (dsonTop)

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do
                           putStr "parse error at "
                           print err
            Right x  -> print x

main :: IO ()
main = do
         args <- getLine
         run dsonTop args
