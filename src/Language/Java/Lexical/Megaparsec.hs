{-# LANGUAGE LambdaCase #-}

module Language.Java.Lexical.Megaparsec
  ( module Text.Megaparsec
  , P
  , runP
  , runP'
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

runP :: Parsec e s c -> (ParseErrorBundle s e -> l) -> s -> Either l c
runP p k raw = first k (parse p "" raw)

-- just get the list of custom errors and pretty printed message,
-- which seems most useful out of other stuff.
runP'
  :: (ShowErrorComponent e, Stream s)
  => ( Parsec e s c
       -> ([e] -> String -> l)
       -> s
       -> Either l c
     )
runP' p k = runP p (k <$> customErrors <*> errorBundlePretty)

customErrors :: (Stream s, Ord e) => ParseErrorBundle s e -> [e]
customErrors eb = case fold (bundleErrors eb) of
  TrivialError {} -> []
  FancyError _ s ->
    concatMap
      (\case
         ErrorCustom e -> [e]
         _ -> [])
      s
