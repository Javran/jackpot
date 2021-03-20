{-# LANGUAGE LambdaCase #-}

module Language.Java.Alex.Wrapper where

import Language.Java.Alex.Token
import Language.Java.Alex.Alex
import Language.Java.Alex.Preprocess

allTokens :: Alex [Token]
allTokens =
  alexMonadScan >>= \case
    EndOfFile -> pure []
    x -> (x :) <$> allTokens

parseAll :: String -> Either String [Token]
parseAll xs = runAlex ys allTokens
  where
    Just ys = preprocess xs
