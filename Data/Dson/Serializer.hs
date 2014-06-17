{-|
Module      : Data.Dson.Serializer
Copyright   : (c) Ezequiel Alvarez 2014
License     : MIT
Maintainer  : welcometothechango@gmail.com
Stability   : provisional
Portability : portable

Provides serializers from Dson to Strings formats.
-}

module Data.Dson.Serializer (dsonToJson) where

import Data.Dson.Parsec
import Data.List

wrapLiteral s = "\"" ++ s ++ "\""
interCommaMap f l = intercalate ", " $ map f l

-- | Takes a Dson value and turns it into a JSON string.
dsonToJson :: Dson -> String
dsonToJson (DSDict list) = "{ " ++ interCommaMap showDict list ++ " }"
    where showDict (key, val) = (wrapLiteral key) ++ ": " ++ (dsonToJson val)

dsonToJson (DSArray list) = "[" ++ interCommaMap dsonToJson list  ++ "]"
dsonToJson (DSString str) = wrapLiteral str
dsonToJson (DSNumber n) = show n
dsonToJson Yes = "true"
dsonToJson No = "false"
dsonToJson Empty = "null"
