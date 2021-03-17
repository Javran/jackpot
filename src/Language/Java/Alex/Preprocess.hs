module Language.Java.Alex.Preprocess where

{-
  This technically has nothing to do with Alex,
  but Java spec demands (3.2. Lexical Translations) that Unicode escape to be performed as
  the first step of lexical analysis, which is easier to do outside of Alex's
  framework.

  Therefore UnicodeEscape is here as a pre-processing step prior to the Alex scan.

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

preprocess :: String -> Maybe String
preprocess = unicodeEscape >=> lineTerminatorNorm
