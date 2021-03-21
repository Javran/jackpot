{-# LANGUAGE LambdaCase #-}

module Language.Java.Lexical.Megaparsec
  ( module Text.Megaparsec
  , P
  , runP
  , customErrors
  )
where

{-
  Our use of megaparsec is on parsing and validating
  small chunks of data that are already recognized by
  Alex - the idea is to accept a wider range of language
  and enforce fine-grained details like "numeric value must fit".
 -}

import Data.Bifunctor
import Data.Foldable
import Data.Void
import Text.Megaparsec

{-
  We don't want to use this custom data type here:
  it's possible but it's burried under ParseErrorBundle,
  which is a PITA to traverse through.
 -}
type P = Parsec Void String

runP :: P a -> (String -> e) -> String -> Either e a
runP p k raw = first (k . errorBundlePretty) (parse p "" raw)

customErrors :: (Stream s, Ord e) => ParseErrorBundle s e -> [e]
customErrors eb = case fold (bundleErrors eb) of
  TrivialError {} -> []
  FancyError _ s ->
    concatMap
      (\case
         ErrorCustom e -> [e]
         _ -> [])
      s
