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
import Numeric
import Text.ParserCombinators.ReadP

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
lineTerminatorNormP :: ReadP String
lineTerminatorNormP =
  concat <$> many (lineTerms <++ munch1 (`notElem` "\r\n"))
  where
    lineTerms =
      "\n"
        <$ (void (char '\n')
              <++ (do
                     _ <- char '\r'
                     -- a bit subtle here,
                     -- as "optional" might create an unwanted branch,
                     -- if we can find a '\n' following it,
                     -- the decision should commit.
                     xs <- look
                     case xs of
                       '\n' : _ -> get >> pure ()
                       _ -> pure ()))

lineTerminatorNorm :: String -> Maybe String
lineTerminatorNorm = runReadP lineTerminatorNormP

dropLastSub :: String -> String
dropLastSub "" = ""
dropLastSub xs =
  if last xs == '\x1a'
    then init xs
    else xs

preprocess :: String -> Maybe String
preprocess = unicodeEscape >=> lineTerminatorNorm >=> pure . dropLastSub