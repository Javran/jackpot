{-# LANGUAGE TypeApplications #-}

module Language.Java.Alex.FloatingPoint where

import Data.Char
import Data.Scientific
import Numeric (readDec, readHex)
import Text.ParserCombinators.ReadP

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

oneOf :: [] Char -> ReadP Char
oneOf xs = satisfy (`elem` xs)

floatingPointLiteralS :: ReadS (Scientific, Bool)
floatingPointLiteralS = readP_to_S (floatingPointLiteral <* eof)

floatTypeSuffix :: ReadP Bool
floatTypeSuffix =
  (True <$ oneOf "dD")
    <++ (False <$ oneOf "fF")

signedInteger :: ReadP Integer
signedInteger = do
  sign <- option 1 ((1 <$ char '+') <++ ((-1) <$ char '-'))
  ds <- filter (/= '_') <$> munch1 (\ch -> isDigit ch || ch == '_')
  [(v, "")] <- pure $ readDec @Integer ds
  pure $ v * sign

floatingPointLiteral
  , decimalFloatingPointLiteral
  , hexadecimalFloatingPointLiteral
    :: ReadP (Scientific, Bool)
floatingPointLiteral =
  hexadecimalFloatingPointLiteral <++ decimalFloatingPointLiteral
decimalFloatingPointLiteral = do
  sig <-
    (do
       -- <before> <dot> [after]
       beforeDot <- digits
       [(bfd, "")] <- pure $ readDec @Integer $ beforeDot
       let intPart :: Scientific
           intPart = fromInteger bfd
       _ <- char '.'
       afterDot <- option "0" digits
       [(afd, "")] <- pure $ readDec @Integer $ afterDot
       let fracPart :: Scientific
           fracPart = scientific afd (- length afterDot)
       pure (intPart + fracPart))
      +++ (do
             -- <dot> <after>
             _ <- char '.'
             afterDot <- option "0" digits
             [(afd, "")] <- pure $ readDec @Integer $ afterDot
             let fracPart :: Scientific
                 fracPart = scientific afd (- length afterDot)
             pure fracPart)
      +++ (do
             -- <before>
             beforeDot <- digits
             [(bfd, "")] <- pure $ readDec @Integer $ beforeDot
             let intPart :: Scientific
                 intPart = fromInteger bfd
             pure intPart)
  decExpon <- option 0 exponentPart
  let expon :: Scientific
      expon =
        -- TODO: this is an Integer to Int conversion, which might overflow
        scientific 1 (fromIntegral decExpon)
  isDouble <- option True floatTypeSuffix
  pure (sig * expon, isDouble)
  where
    digits = filter (/= '_') <$> munch1 (\ch -> isDigit ch || ch == '_')
    exponentPart = oneOf "eE" *> signedInteger
hexadecimalFloatingPointLiteral = do
  sig <- hexSignificand
  binExpon <- binaryExponent
  let expon :: Scientific
      expon = 2 ^ binExpon
  isDouble <- option True floatTypeSuffix
  pure (sig * expon, isDouble)
  where
    hexSignificand :: ReadP Scientific
    hexSignificand = do
      _ <- char '0'
      _ <- oneOf "xX"
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
    binaryExponent =
      oneOf "pP" *> signedInteger
