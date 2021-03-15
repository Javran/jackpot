{-# LANGUAGE OverloadedStrings, BinaryLiterals #-}

module Language.Java.Alex.LexerSpec where

{-
  Note that this does not directly import Lexer but rather the Wrapper module,
  which is more convenient to work with.
 -}

import Control.Monad
import Data.Either
import Data.String
import Language.Java.Alex.Token
import Language.Java.Alex.Wrapper
import Test.Hspec
import Text.Read

parseOk :: String -> [Token] -> Expectation
parseOk raw expected =
  parseAll (fromString raw) `shouldBe` Right expected

parseFail :: String -> Expectation
parseFail raw =
  parseAll (fromString raw) `shouldSatisfy` isLeft

spec :: Spec
spec = do
  describe "Boolean and Null literals" $ do
    specify "NullLiteral" $
      parseOk "null" [NullLiteral]

    specify "BooleanLiteral" $
      parseOk "true false" $ BooleanLiteral <$> [True, False]

  describe "IntegerLiteral" $ do
    let int v = IntegerLiteral v False
        long v = IntegerLiteral v True

    describe "DecimalIntegerLiteral" $ do
      specify "0" $
        parseOk "  0  " [int 0]
      specify "non-zero single digit" $ do
        forM_ [1 .. 9] $ \x ->
          parseOk (show x) [int x]
      specify "many digits" $ do
        parseOk "90" [int 90]
        parseOk "12345" [int 12345]
      specify "interleave with underscores" $ do
        parseOk "1__2__34__5" [int 12345]
        parseOk "12__34_____5" [int 12345]
        parseFail "_12__34__5"
      specify "IntegerTypeSuffix" $ do
        parseOk "1_2__34__5L" [long 12345]
        parseOk "54321l" [long 54321]
        parseFail "1_2_L"

    describe "HexIntegerLiteral" $ do
      specify "0x / 0X" $ do
        parseOk "0xdeadbeef" [int 0xdeadbeef]
        parseOk "0XDEADBEEF" [int 0xDEADBEEF]
      specify "single digit" $ do
        forM_ (['0' .. '9'] <> "ABCDEF" <> "abcdef") $ \ch -> do
          let inp = "0x" <> [ch]
              Right expected = readEither inp
          parseOk inp [int expected]
      specify "more than one digit" $ do
        parseOk "0xEA" [int 0xEA]
        parseOk "0x01234" [int 0x1234]
        parseFail "0xEFG"
      specify "interleave with underscores" $ do
        parseFail "0x_EA"
        parseFail "0xEA_"
        parseOk "0xA___B___C__D__E" [int 0xABCDE]
      specify "IntegerTypeSuffix" $ do
        parseOk "0x1_2__34__5L" [long 0x12345]
        parseOk "0X54321l" [long 0x54321]
        parseFail "0x1_2_L"

    describe "OctalIntegerLiteral" $ do
      specify "one digit" $ do
        forM_ ['0' .. '7'] $ \ch -> do
          let Right expected = readEither ("0o" <> [ch])
          parseOk ('0' : [ch]) [int expected]
        parseFail "08"
        parseFail "09"
      specify "many digits" $ do
        parseOk "0123456" [int 0o123456]
        parseOk "000666" [int 0o666]
        parseFail "0090666"
        parseFail "0001200238"
      specify "interleave with underscores" $ do
        parseFail "0_1"
        parseFail "01_"
        parseOk "0123_45" [int 0o12345]
        parseOk "01__23_4____5" [int 0o12345]
      specify "IntegerTypeSuffix" $ do
        parseOk "01_2__34__5L" [long 0o12345]
        parseOk "054321l" [long 0o54321]
        parseFail "0x1_2_L"

    describe "BinaryIntegerLiteral" $ do
      specify "0b / 0B" $ do
        parseOk "0b1011" [int 0b1011]
        parseOk "0B1011" [int 0b1011]
      specify "digits" $ do
        parseOk "0b1001111" [int 0b1001111]
        parseOk "0b000111101" [int 0b111101]
      specify "interleave with underscores" $ do
        parseFail "0b_1"
        parseFail "0b1_"
        parseOk "0b1" [int 1]
        parseOk "0b1__0" [int 0b10]
        parseOk "0b111_____01" [int 0b11101]
        parseOk "0b111___1_1__01" [int 0b1111101]
