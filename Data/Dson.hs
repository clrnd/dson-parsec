{-|
Module      : Data.Dson
Copyright   : (c) Ezequiel Alvarez 2014
License     : MIT
Maintainer  : welcometothechango@gmail.com
Stability   : provisional
Portability : portable

Main module. Has a test for the parser and the serializer.
-}

module Data.Dson where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Dson.Parsec
import Data.Dson.Serializer (dsonToJson)

run :: Parser Dson -> String -> IO ()
run p input = case (parse p "" input) of
                  Left err -> putStr "parse error at " >> print err
                  Right x  -> print $ dsonToJson x

-- | Reads a DSON string, parses it to a Dson value and serializes it back to JSON.
main :: IO ()
main = run dsonTop =<< getLine
