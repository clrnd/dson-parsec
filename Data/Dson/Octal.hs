module Data.Dson.Octal (octal) where

import Data.Dson.Lexer

import Control.Applicative
import Data.Char (digitToInt, isOctDigit)
import Data.Maybe (fromMaybe)
import Text.Parsec (satisfy, char)
import Text.Parsec.String (Parser)

octalToDouble :: (String, Maybe String, Maybe (Double, String)) -> Double
octalToDouble (int, m_dec, m_exp) = (parseOctStrs int dec) * (8 ** (sign * (parseOctStrs exp "")))
    where (sign, exp) = fromMaybe (1, "0") m_exp
          dec = fromMaybe "" m_dec

-- Conversion according to http://en.wikipedia.org/wiki/Octal#Octal_to_decimal_conversion
-- Decimal part has negative exponent.
parseOctStrs :: String -> String -> Double
parseOctStrs  intPart decPart =
    snd $ foldl expAndAdd (initlLen, 0) (intPart ++ decPart)
    where initlLen = fromIntegral . pred . length $ intPart
          expAndAdd (exp, acc) b = (pred exp, acc + (fromIntegral . digitToInt $ b) * (8 ** exp))

factor :: Parser Double
factor = (char '-' *> pure (-1)) <|> (char '+' *> pure 1) <|> pure 1

octalParts :: Parser (String, Maybe String, Maybe (Double, String))
octalParts = (,,) <$> some octDigit <*> optional octalDecimals <*> optional octalExponent
    where octDigit = satisfy isOctDigit
          octalDecimals = symbol "." *> some octDigit
          octalExponent = (,) <$> (veryVERY *> factor) <*> some octDigit
          veryVERY = symbol "very" <|> symbol "VERY"

octal :: Parser Double
octal = (*) <$> factor <*> (octalToDouble <$> octalParts)
