module Language.Java.Lexical.PlatformFunctionSpec where

import Language.Java.Lexical.PlatformFunction
import Test.Hspec
import Control.Monad

spec :: Spec
spec = do
  describe "isJavaIdentifierStart" $ do
    specify "examples"$ do
      forM_ "a_\xB86\x1CEF" $ \ch ->
        isJavaIdentifierStart ch `shouldBe` True
      forM_ "6\x09D7" $ \ch ->
        isJavaIdentifierStart ch `shouldBe` False
  describe "isJavaIdentifierPart" $ do
    specify "examples"$ do
      forM_ "a_6\x09D7\xB86\xD4C" $ \ch ->
        isJavaIdentifierPart ch `shouldBe` True
      forM_ "\xD49\x201F" $ \ch ->
        isJavaIdentifierPart ch `shouldBe` False
