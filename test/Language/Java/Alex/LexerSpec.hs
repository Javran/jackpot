{-# LANGUAGE OverloadedStrings #-}

module Language.Java.Alex.LexerSpec where

{-
  Note that this does not directly import Lexer but rather the Wrapper module,
  which is more convenient to work with.
 -}

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Language.Java.Alex.Token
import Language.Java.Alex.Wrapper
import Test.Hspec

parseOk :: BSL.ByteString -> [Token] -> Expectation
parseOk raw expected =
  parseAll raw `shouldBe` Right expected

spec :: Spec
spec = do
  describe "Boolean and Null literals" $ do
    specify "NullLiteral" $
      parseOk "null" [NullLiteral]

    specify "BooleanLiteral" $
      parseOk "true false" $ BooleanLiteral <$> [True, False]

  describe "IntegerLiteral" $
    describe "DecimalIntegerLiteral" $ do
      specify "0" $
        parseOk "  0  " [Todo "0"]
      specify "non-zero single digit" $ do
        parseOk "1" [Todo "1"]
        parseOk "7" [Todo "7"]
      specify "many digits" $ do
        parseOk "90" [Todo "90"]
        parseOk "12345" [Todo "12345"]
      specify "interleave with underscores" $ do
        parseOk "1__2__34__5" [Todo "1__2__34__5"]
        parseOk "12__34__5" [Todo "12__34__5"]
        parseAll "_12__34__5" `shouldSatisfy` isLeft
