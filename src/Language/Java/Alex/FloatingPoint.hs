{-# LANGUAGE TypeApplications #-}

module Language.Java.Alex.FloatingPoint where

import Data.Char
import Data.Scientific
import Numeric (readHex)
import Text.ParserCombinators.ReadP
import Text.Read (readEither)

{-
  Parsing floating point literals.

  Note that parser might accept a wider range of inputs than Java spec,
  this is fine since we are only taking inputs that are already
  recognized as proper Java tokens.
 -}

{-
  An example for hex floating point:

  0xAABB.CDEp12 means:

  0xAABBCDE / (16^3) * (2^12)

  which should be evaluated to: 1.79027166e8

 -}

floatingPointLiteralS :: ReadS (Scientific, Bool)
floatingPointLiteralS = readP_to_S (floatingPointLiteral <* eof)

floatingPointLiteral
  , decimalFloatingPointLiteral
  , hexadecimalFloatingPointLiteral
    :: ReadP (Scientific, Bool)
floatingPointLiteral =
  hexadecimalFloatingPointLiteral <++ decimalFloatingPointLiteral
decimalFloatingPointLiteral = pfail -- TODO
hexadecimalFloatingPointLiteral = do
  sig <- hexSignificand
  binExpon <- binaryExponent
  let expon :: Scientific
      expon = 2 ^ binExpon
  isDouble <- option True ((True <$ satisfy (`elem` "dD")) <++ (False <$ satisfy (`elem` "fF")))
  pure (sig * expon, isDouble)
  where
    hexSignificand :: ReadP Scientific
    hexSignificand = do
      _ <- char '0'
      _ <- satisfy (`elem` "xX")
      (do
         beforeDot <- hexDigits <* optional (char '.')
         [(v, "")] <- pure $ readHex @Integer $ beforeDot
         pure (fromInteger v))
        +++ (do
               beforeDot <- option "0" hexDigits
               [(bfd, "")] <- pure $ readHex @Integer $ beforeDot
               let intPart :: Scientific
                   intPart = fromInteger bfd
               _ <- char '.'
               afterDot <- hexDigits
               [(afd, "")] <- pure $ readHex @Integer $ afterDot
               let fracPart :: Scientific
                   fracPart = fromInteger afd / (16 ^ length afterDot)
               pure (intPart + fracPart))
      where
        hexDigits =
          filter (/= '_')
            <$> munch1 (\ch -> isHexDigit ch || ch == '_')
    binaryExponent :: ReadP Integer
    binaryExponent = do
      _ <- satisfy (`elem` "pP")
      sign <- option 1 ((1 <$ char '+') <++ ((-1) <$ char '-'))
      ds <- filter (/= '_') <$> munch1 (\ch -> isDigit ch || ch == '_')
      Right v <- pure $ readEither @Integer ds
      pure $ v * sign
