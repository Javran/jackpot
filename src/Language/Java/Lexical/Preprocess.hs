{-# LANGUAGE LambdaCase #-}

module Language.Java.Lexical.Preprocess where

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

import Control.Monad
import Data.Char
import Data.Function
import Data.List.Extra
import Numeric
import Text.ParserCombinators.ReadP
import Control.Arrow

unicodeEscapeP :: ReadP String
unicodeEscapeP =
  concat
    <$> many
      ((do
          ss <- munch1 (== '\\')
          if even (length ss)
            then pure ss -- not eligible
            else do
              us <- munch (== 'u')
              if null us
                then pure ss
                else do
                  ds <- replicateM 4 (satisfy isHexDigit)
                  [(v, "")] <- pure $ readHex ds
                  pure $ drop 1 ss <> [chr v])
         <++ munch1 (/= '\\'))

runReadP :: ReadP a -> String -> Maybe a
runReadP p raw = case readP_to_S (p <* eof) raw of
  [(v, "")] -> Just v
  _ -> Nothing

unicodeEscape :: String -> Maybe String
unicodeEscape = runReadP unicodeEscapeP

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
