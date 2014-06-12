module Data.Dson.Octal (octal) where

import Data.Dson.Lexer

import Control.Applicative
import Data.Char (digitToInt, isOctDigit)
import Data.Maybe (fromMaybe)
import Text.Parsec (satisfy, char)
import Text.Parsec.String (Parser)

factor :: Parser Double
factor = (char '-' *> pure (-1)) <|> (char '+' *> pure 1) <|> pure 1

octalParts :: Parser (String, Maybe String, Maybe (Double, String))
octalParts = (,,) <$> some octDigit <*> optional octalDecimals <*> optional octalExponent
    where octDigit = satisfy isOctDigit
          octalDecimals = symbol "." *> some octDigit
          octalExponent = (,) <$> (veryVERY *> factor) <*> some octDigit
          veryVERY = symbol "very" <|> symbol "VERY"

octalToDouble :: (String, Maybe String, Maybe (Double, String)) -> Double
octalToDouble (int, m_dec, m_exp) = (parseOctStrs int dec) ** (sign * (parseOctStrs exp ""))
    where sign_exp = fromMaybe (1, "1") m_exp
          sign = fst sign_exp
          exp = snd sign_exp
          dec = fromMaybe "" m_dec

-- Conversion according to http://en.wikipedia.org/wiki/Octal#Octal_to_decimal_conversion
-- Decimal part has negative exponent.
parseOctStrs :: String -> String -> Double
parseOctStrs  intPart decPart =
    snd $ foldl expAndAdd (initlLen, 0) (intPart ++ decPart)
    where initlLen = fromIntegral . pred . length $ intPart
          expAndAdd (exp, acc) b = (pred exp, acc + (fromIntegral . digitToInt $ b) * (eight ** exp))
          eight = fromIntegral 8 :: Double

octal = (*) <$> factor <*> (octalToDouble <$> octalParts)
