module Data.Dson.Serializer (dsonToJson) where

import Data.Dson.Parsec
import Data.List

wrapLiteral s = "\"" ++ s ++ "\""
interCommaMap f l = intercalate ", " $ map f l

dsonToJson :: Dson -> String
dsonToJson (DSDict list) = "{ " ++ interCommaMap showDict list ++ " }"
    where showDict (key, val) = (wrapLiteral key) ++ ": " ++ (dsonToJson val)

dsonToJson (DSArray list) = "[" ++ interCommaMap dsonToJson list  ++ "]"
dsonToJson (DSString str) = wrapLiteral str
dsonToJson (DSNumber n) = show n
dsonToJson Yes = "true"
dsonToJson No = "false"
dsonToJson Empty = "null"
