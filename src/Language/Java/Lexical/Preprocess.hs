{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Language.Java.Lexical.Preprocess
  ( unicodeEscape
  , lineTerminatorNorm
  , dropLastSub
  , preprocess
  )
where

{-
  Java spec requires that:

  - (§3.2) Unicode escape to be performed as the first step of lexical analysis.
  - (§3.2) Line terminators to be treated the same.
  - (§3.3) There may be an optional ASCII Sub as the last character of input stream,
    which is to be ignored.

  This module serves as a preprocessing step to do those things prior to
  passing input to Alex.

  According to my understading of the spec spec,
  for \u0000 ~ \uFFFF, we just need to map them to corresponding
  chars, and we don't need to deal with surrogate pairs, which simplifies
  preprocessing.
 -}

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List.Extra
import Language.Java.PError
import Numeric
import Text.Megaparsec

type P = Parsec PError String

unicodeEscapeP :: P String
unicodeEscapeP = concat <$> many (nonSlash <|> mayEscape)
  where
    nonSlash = takeWhile1P (Just "not slash") (/= '\\')
    mayEscape = do
      ss <- takeWhile1P (Just "potential escape") (== '\\')
      if even (length ss)
        then -- not eligible
          pure ss
        else do
          us <- takeWhileP (Just "'u'") (== 'u')
          if null us
            then -- eligible but without any 'u'
              pure ss
            else do
              ds <- takeP (Just "hex value") 4
              let err = fail $ "invalid hex value: " <> ds
              unless (all isHexDigit ds) err
              case readHex @Int ds of
                [(v, "")] ->
                  pure $ drop 1 ss <> [chr v]
                _ -> err

unicodeEscape :: String -> Maybe String
unicodeEscape = parseMaybe unicodeEscapeP

{-
  Normalizes CR+LF / CR / LF to LF (\n).
  This plays nicely with Alex since in Alex,
  regular expression `.` means anything but \n.
 -}
lineTerminatorNorm :: String -> String
lineTerminatorNorm =
  fix
    (\f -> \case
       '\r' : '\n' : ys -> '\n' : f ys
       '\r' : ys -> '\n' : f ys
       y : ys -> y : f ys
       [] -> [])

dropLastSub :: String -> String
dropLastSub xs = case zs of
  ['\x1a'] -> ys
  _ -> xs
  where
    (ys, zs) = splitAtEnd 1 xs

preprocess :: String -> Maybe String
preprocess = unicodeEscape >=> pure . (lineTerminatorNorm >>> dropLastSub)
