{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Language.Java.Alex.Wrapper where

import Control.Carrier.State.Strict
import Control.Monad.Trans.Except
import Language.Java.Alex.Alex
import Language.Java.Alex.Preprocess
import Language.Java.Alex.Token

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
