{-# LANGUAGE LambdaCase #-}

module Language.Java.Alex.Wrapper where

import Language.Java.Alex.Token
import Language.Java.Alex.Lexer
import qualified Data.ByteString.Lazy as BSL

allTokens :: Alex [Token]
allTokens =
  alexMonadScan >>= \case
    EndOfFile -> pure []
    x -> (x :) <$> allTokens

parseAll :: BSL.ByteString -> Either String [Token]
parseAll xs = runAlex xs allTokens
