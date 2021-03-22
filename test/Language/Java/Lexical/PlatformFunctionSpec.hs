module Language.Java.Lexical.PlatformFunctionSpec where

import Language.Java.Lexical.PlatformFunction
import Test.Hspec

spec :: Spec
spec = do
  let allChars :: [Char]
      allChars = [minBound .. maxBound]
  describe "isJavaIdentifierStart" $
    specify "length check" $
      length (filter isJavaIdentifierStart allChars) `shouldBe` 131549
  describe "isJavaIdentifierPart" $
    specify "length check" $
      length (filter isJavaIdentifierPart allChars) `shouldBe` 134698
