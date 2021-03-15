{-# LANGUAGE TypeApplications #-}

module Language.Java.Alex.FloatingPoint where

import Data.Char
import Text.ParserCombinators.ReadP
import Text.Read (readEither)

{-
  Parsing floating point literals.

  Note that parser might accept a wider range of inputs than Java spec,
  this is fine since we are only taking inputs that are already
  recognized as proper Java tokens.
 -}

floatingPointLiteral
  , decimalFloatingPointLiteral
  , hexadecimalFloatingPointLiteral
    :: ReadP ((Integer, Integer), Integer, Bool)
floatingPointLiteral =
  hexadecimalFloatingPointLiteral <++ decimalFloatingPointLiteral
decimalFloatingPointLiteral = pfail
hexadecimalFloatingPointLiteral = do
  sig <- hexSignificand
  binExpon <- binaryExponent
  isDouble <- option True ((True <$ satisfy (`elem` "dD")) <++ (False <$ satisfy (`elem` "fF")))
  pure (sig, binExpon, isDouble)
  where
    hexSignificand :: ReadP (Integer, Integer)
    hexSignificand = do
      _ <- char '0'
      _ <- satisfy (`elem` "xX")
      (do
         beforeDot <- hexDigits <* optional (char '.')
         Right v <- pure $ readEither @Integer $ "0x" <> beforeDot
         pure (v, 0))
        <++ (do
               beforeDot <- option "0" hexDigits
               Right bfd <- pure $ readEither @Integer $ "0x" <> beforeDot
               _ <- char '.'
               afterDot <- hexDigits
               -- TODO: this is incorrect: ".0001" and ".1" will be parsed the same - length must be taken into account.
               Right afd <- pure $ readEither @Integer $ "0x" <> afterDot
               pure (bfd, afd))
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
