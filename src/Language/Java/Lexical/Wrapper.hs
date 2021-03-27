{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Language.Java.Lexical.Wrapper where

import Control.Carrier.State.Strict
import Control.Monad.Trans.Except
import Language.Java.Lexical.Alex
import Language.Java.Lexical.Preprocess
import Language.Java.Lexical.Token
import Language.Java.PError

allTokens :: Alex sig m => m [Token]
allTokens =
  alexMonadScan >>= \case
    EndOfFile -> pure []
    x -> (x :) <$> allTokens

lexer :: Alex sig m => (Token -> m a) -> m a
lexer k = alexMonadScan >>= k

parseAll :: String -> Either PError [Token]
parseAll xs = do
  ys <- preprocess xs
  fmap snd
    . run
    . runExceptT @PError
    . runState @AlexState (alexInitState ys)
    $ allTokens
