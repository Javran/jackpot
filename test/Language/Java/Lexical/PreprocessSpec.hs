{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Lexical.PreprocessSpec where

import Data.Char
import Language.Java.Lexical.Preprocess
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "unicodeEscape" $ do
    let parseOk inp expected = unicodeEscape inp `shouldBe` Just expected
        parseSame s = parseOk s s
        parseFail inp = unicodeEscape inp `shouldBe` Nothing
    specify "normal" $ do
      parseSame "AABBB"
      parseSame "AA😹😹 "
    specify "simple escape" $ do
      parseOk [r|\u00AA|] [chr 0x00AA]
      parseOk [r|\uAbCD|] [chr 0xABCD]
      parseOk [r|\u6655E|] [chr 0x6655, 'E']
    specify "many 'u's" $ do
      parseOk [r|\uuuuuuuuu00AA|] [chr 0x00AA]
      parseOk [r|\uuuuuuuuuuu1234|] [chr 0x1234]
    specify "odd slashes are eligible" $ do
      parseOk [r|\\\\\u1234|] $ [r|\\\\|] <> [chr 0x1234]
      parseOk [r|AAA123\\\\\u12345|] $ [r|AAA123\\\\|] <> [chr 0x1234] <> "5"
    specify "even slashes are not eligible" $ do
      parseSame [r|\\u1234|]
      parseSame [r|A\\\\u1234B|]
    specify "odd slashes but no code" $
      parseSame [r|\\\\\1234|]
    specify "interleaved" $ do
      parseOk
        [r|!!!$\u0123\\uFFEEsss\\\uuuAAFFD|]
        $ [r|!!!$|] <> [chr 0x0123] <> [r|\\uFFEEsss\\|] <> [chr 0xAAFF] <> "D"
      parseOk
        [r|\\\u0123\\\\uu!sss\\\uuuAAFFD|]
        $ [r|\\|] <> [chr 0x0123] <> [r|\\\\uu!sss\\|] <> [chr 0xAAFF] <> "D"
    specify "should fail" $ do
      parseFail [r|\uu|]
      parseFail [r|\\\uuuu|]
      parseFail [r|AAA\uABG1|]
      parseFail [r|\uABCD\uAAVE|]
  describe "lineTerminatorNorm" $ do
    let parseOk inp expected = lineTerminatorNorm inp `shouldBe` Just expected
    specify "examples" $ do
      parseOk "ABCD" "ABCD"
      parseOk "\n\n\n" "\n\n\n"
      parseOk "\r\r\r\r" "\n\n\n\n"
      parseOk "\n\r" "\n\n"
      parseOk "\r\n" "\n"
      parseOk
        "CDEF\r\nA\n\rB\r\r\nVCCCC\r\r\n\n!!\r\rGH"
        "CDEF\nA\n\nB\n\nVCCCC\n\n\n!!\n\nGH"
  describe "dropLastSub" $ do
    let parseEq inp expected = dropLastSub inp `shouldBe` expected
    specify "example" $ do
      parseEq "[]\x1A" "[]"
      parseEq "fooo" "fooo"
