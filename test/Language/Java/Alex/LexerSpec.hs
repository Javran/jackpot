{-# LANGUAGE OverloadedStrings #-}

module Language.Java.Alex.LexerSpec where

{-
  Note that this does not directly import Lexer but rather the Wrapper module,
  which is more convenient to work with.
 -}

import qualified Data.ByteString.Lazy as BSL
import Language.Java.Alex.Token
import Language.Java.Alex.Wrapper
import Test.Hspec

parseOk :: BSL.ByteString -> [Token] -> Expectation
parseOk raw expected =
  parseAll raw `shouldBe` Right expected

spec :: Spec
spec =
  describe "Boolean and Null literals" $ do
    specify "NullLiteral" $
      parseOk "null" [NullLiteral]

    specify "BooleanLiteral" $
      parseOk "true false" $ BooleanLiteral <$> [True, False]
