{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Language.Java.Lexical.Wrapper where

import Control.Carrier.State.Strict
import Control.Monad.Trans.Except
import Language.Java.Lexical.Alex
import Language.Java.Lexical.Preprocess
import Language.Java.Lexical.Token

allTokens :: Alex sig m => m [Token]
allTokens =
  alexMonadScan >>= \case
    EndOfFile -> pure []
    x -> (x :) <$> allTokens

parseAll :: String -> Either AlexError [Token]
parseAll xs =
  fmap snd
    . run
    . runExceptT @AlexError
    . runState @AlexState (alexInitState ys)
    $ allTokens
  where
    Just ys = preprocess xs
