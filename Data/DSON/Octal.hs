module Data.Dson.Octal (octalToDouble) where

import Data.Char (digitToInt)

octalToDouble :: (String, Maybe String, Maybe (Double, String)) -> Double
octalToDouble (intPart, Just decPart, Just (factor, exponent)) =
    (parseOctStrs intPart decPart) ** (factor * (parseOctStrs exponent ""))
octalToDouble (a, Nothing, Just b) = octalToDouble (a, Just "", Just b)
octalToDouble (a, Just b, Nothing) = octalToDouble (a, Just b, Just (1, "1"))
octalToDouble (a, Nothing, Nothing) = octalToDouble (a, Just "", Just (1, "1"))

parseOctStrs :: String -> String -> Double
parseOctStrs  intPart decPart =
    snd $ foldl expAndAdd (initlLen, 0) (intPart ++ decPart)
    where initlLen = fromIntegral . pred . length $ intPart
          expAndAdd (exp, acc) b = (pred exp, acc + (fromIntegral . digitToInt $ b) * (eight ** exp))
          eight = fromIntegral 8 :: Double

