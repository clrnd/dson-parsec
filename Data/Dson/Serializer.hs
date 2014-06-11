module Data.Dson.Serializer (dsonToJson) where

import Data.Dson.Parsec

import Data.List

dsonToJson :: Dson -> String
dsonToJson (DSDict list) = "{ " ++ (concat . (intersperse ", ") . (map showDict)) list ++ " }"
    where showDict (key, val) = (wrapLiteral key) ++ ": " ++ (dsonToJson val)

dsonToJson (DSArray list) = "[" ++ (concat . (intersperse ", ") . (map dsonToJson)) list ++ "]"

dsonToJson (DSString str) = wrapLiteral str
dsonToJson (DSNumber n) = show n
dsonToJson Yes = "true"
dsonToJson No = "false"
dsonToJson Empty = "null"

wrapLiteral s = "\"" ++ s ++ "\""
