module Language.Java.Lexical.PlatformFunctionSpec where

import Language.Java.Lexical.PlatformFunction
import Test.Hspec

spec :: Spec
spec = do
  let allChars :: [Char]
      allChars = [minBound .. maxBound]
  {-
    TODO: it is the job of unicode-general-category to check its completeness,
    rather than this package. Instead we should just have some examples
    instead of trying to have a complete coverage over all codepoints.
   -}
  describe "isJavaIdentifierStart" $
    specify "length check" $
      length (filter isJavaIdentifierStart allChars) `shouldBe` 131549
  describe "isJavaIdentifierPart" $
    specify "length check" $
      length (filter isJavaIdentifierPart allChars) `shouldBe` 134698
