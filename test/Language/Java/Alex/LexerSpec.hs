{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Alex.LexerSpec where

{-
  Note that this does not directly import Lexer but rather the Wrapper module,
  which is more convenient to work with.
 -}

import Control.Monad
import Data.Either
import Data.Scientific
import Data.String
import Language.Java.Alex.Token
import Language.Java.Alex.Wrapper
import Test.Hspec
import Text.RawString.QQ
import Text.Read

parseOk :: String -> [Token] -> Expectation
parseOk raw expected =
  parseAll (fromString raw) `shouldBe` Right expected

parseFail :: String -> Expectation
parseFail raw =
  parseAll (fromString raw) `shouldSatisfy` isLeft

spec :: Spec
spec = do
  let ident = Identifier
  describe "whitespace" $ do
    specify "SP HT LF FF CR" $
      parseOk " \t\n\f\r" []
    {-
      Java spec does not mention about VT but it's in Alex's $white definition,
      so we need to exclude that manually.
     -}
    specify "no VT" $ do
      parseFail "\v"
      parseFail " \t\n\f\r\v"

  describe "Boolean and Null literals" $ do
    specify "NullLiteral" $
      parseOk "null" [NullLiteral]

    specify "BooleanLiteral" $
      parseOk "true false" $ BooleanLiteral <$> [True, False]
  let int v = IntegerLiteral v False
      long v = IntegerLiteral v True

  describe "TraditionalComment" $ do
    let n = NullLiteral
        t = BooleanLiteral True
        f = BooleanLiteral False
    specify "example set #0" $ do
      parseOk "1 /**/" [int 1]
      parseOk "2/**/" [int 2]
      parseOk "/**/ 3" [int 3]
      parseOk "/* this comment /* // /** ends here: */ 4" [int 4]
      parseOk "/* null */ false /**/null// null" [f, n]
      parseFail "/*****null"
      parseFail " /*/"
      parseFail "/*null*/ /*/"
    specify "example set #1" $ do
      parseFail "/*/null"
      parseOk "/**/2" [int 2]
      parseOk "/***/3" [int 3]
      parseOk "/****/4" [int 4]
      parseOk "/*****/5" [int 5]
      parseOk "/******/6" [int 6]
      parseOk "/***!***/7" [int 7]
      parseOk "/***!****/8" [int 8]
      parseOk "9/* * * * */null" [int 9, n]
    specify "anything inside block comments" $
      parseOk "true/* \v */false" [t, f]
    specify "no nested comments" $
      parseFail "/* no /* nesting */ */"
    specify "multiline" $ do
      parseOk
        [r|
                /*
                 * comment
                 */true
                |]
        [t]
      parseOk
        [r|true
                /**********
                 * comment
                 **********/false
                |]
        [t, f]
      parseOk
        [r|null
                /**********
                 **********/
                 false
                |]
        [n, f]

  {-
    TODO: Java spec requires an out-of-range literal to be
    identified as compliation error,
    which is not yet implemented here.

    We'll go back and revisit how to implement that once we
    having some notion of "irecoverable failure" available.
   -}
  describe "IntegerLiteral" $ do
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
        parseOk "_12__34__5" [ident "_12__34__5"]
      specify "IntegerTypeSuffix" $ do
        parseOk "1_2__34__5L" [long 12345]
        parseOk "54321l" [long 54321]
        parseFail "1_2_L"
        parseOk "0l" [long 0]
        parseOk "4L" [long 4]

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
        parseOk "01L" [long 1]
        parseOk "03l" [long 3]

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

  describe "FloatingPointLiteral" $ do
    let float v = FloatingPointLiteral v False
        double v = FloatingPointLiteral v True
    describe "DecimalFloatingPointLiteral" $ do
      specify "Digits . [Digits] [ExponentPart] [FloatTypeSuffix]" $ do
        parseOk "1234." [double 1234]
        parseOk "1234.56" [double 1234.56]
        parseOk "1234e-2" [double 1234e-2]
        parseOk "4e+8" [double 4e8]
        parseOk "14E8" [double 14e8]
        parseOk "14f" [float 14]
        parseOk "8899d" [double 8899]
        parseOk "1122.3344E+55f" [float 1122.3344e55]
        parseOk "1__12__2.3_34_4E+5___5D" [double 1122.3344e55]
        parseOk "_1__12__2.3_34_4E+5___5D" [ident "_1__12__2", double 3.344e54]
        parseFail "1_.33E+55D"
        parseFail "1._33E+55D"
        parseOk "1.33E+55_D" [double 1.33e55, ident "_D"]
        parseOk "6.022137e+23f" [float 6.022137e23]

      specify ". Digits [ExponentPart] [FloatTypeSuffix]" $ do
        parseOk ".1_3_5" [double 0.135]
        parseOk ".4e1" [double 0.4e1]
        parseOk ".24e+1" [double 0.24e1]
        parseOk ".33E-7" [double 0.33e-7]
        parseOk ".33E-7__7" [double 0.33e-77]
        parseOk ".789f" [float 0.789]
        parseOk ".789F" [float 0.789]
        parseOk ".789d" [double 0.789]
        parseOk ".789D" [double 0.789]
        parseOk ".123e-45d" [double 0.123e-45]

      specify "Digits ExponentPart [FloatTypeSuffix]" $ do
        parseOk "123e3" [double 123e3]
        parseOk "34e-2" [double 34e-2]
        parseOk "22e+33F" [float 22e33]

      specify "Digits [ExponentPart] FloatTypeSuffix" $ do
        parseOk "10d" [double 10]
        parseOk "1234f" [float 1234]

    describe "HexadecimalFloatingPointLiteral" $ do
      let hexAux :: Integer -> Int -> Int -> Scientific
          hexAux hSignificand shift binExpon =
            -- note that shift must be non-negative.
            fromInteger hSignificand * (1 / 16 ^ shift) * expon
            where
              expon = if binExpon >= 0 then 2 ^ binExpon else 1 / (2 ^ (- binExpon))

      describe "HexSignificand BinaryExponent [FloatTypeSuffix]" $ do
        specify "HexSignificand = HexNumeral [.]" $ do
          parseOk "0XC0FFEEP-32" [double $ hexAux 0xC0FFEE 0 (-32)]
          parseOk "0x1234.p+5" [double $ hexAux 0x1234 0 5]
          parseOk "0xDEAD____BEEF.p0f" [float $ hexAux 0xDEADBEEF 0 0]
          parseOk "0xC_D_E.p1_2D" [double $ hexAux 0xCDE 0 12]

        specify "HexSignificand = 0X / 0x [HexDigits] . HexDigits" $ do
          parseOk "0xAABB.CDEp12" [double $ hexAux 0xAABBCDE 3 12]
          parseOk "0X.AC01Dp+3f" [float $ hexAux 0xAC01D 5 3]
          parseOk "0X.66_77_88P-54D" [double $ hexAux 0x667788 6 (-54)]
  describe "Keyword or Identifier" $ do
    specify "examples" $
      parseOk
        "_id _ throw byte final"
        [Identifier "_id", KwSymbolUnderscore, KwThrow, KwByte, KwFinal]
    specify "spec examples" $ do
      let raw = "String i3 αρετη MAX_VALUE isLetterOrDigit"
      parseOk raw $ fmap ident (words raw)
  let char = CharacterLiteral
      str = StringLiteral
  describe "CharacterLiteral" $ do
    specify "SingleCharacter" $ do
      parseOk [r|'r'|] [char 'r']
      parseOk [r|'!'|] [char '!']
      parseFail [r|''|]
      parseFail "'\n'"
      parseFail [r|'\u000a'|]
    specify "Supplementary characters" $
      parseFail [r|'😹'|]
    specify "EscapeSequence" $ do
      parseOk [r|'\r'|] [char '\r']
      parseOk [r|'\n'|] [char '\n']
      parseOk [r|'\\'|] [char '\\']
      parseOk [r|'\''|] [char '\'']
      parseOk [r|'\s'|] [char ' ']
      parseOk [r|'\0'|] [char '\0']
      parseOk [r|'\01'|] [char '\1']
      parseOk [r|'\002'|] [char '\2']
      parseOk [r|'\101'|] [char 'A']
      parseOk [r|'\377'|] [char '\o377']
      parseFail [r|\08|]
      parseFail [r|'\400'|]
      parseFail [r|'|]
  describe "StringLiteral" $ do
    specify "simple" $ do
      parseOk [r|"normal@string!"|] [str "normal@string!"]
      {-
        Unicode stuff: supplementary characters are not allowed
        and surrogate pair should be used instead.
       -}
      parseOk [r|"\uD83D\uDE39"|] [str "\xd83d\xde39"]
      parseFail [r|"😹"|]
      parseFail "\"\n\""
      parseFail [r|"\u000a"|]
      parseFail [r|"unbalanced|]
    specify "mix" $ do
      parseOk [r|"A\102C\u0044"|] [str "ABCD"]
      parseOk [r|"\n\r123\r"|] [str "\n\r123\r"]
  describe "TextBlock" $ do
    specify "spec examples" $ do
      parseOk
        [r|"""
           winter"""|]
        [str "winter"]
      parseOk
        [r|"""
           winter
           """|]
        [str "winter\n"]
      parseOk
        [r|"""
           Hi, "Bob"
           """|]
        [str "Hi, \"Bob\"\n"]
      parseOk
        [r|"""
           Hi,
            "Bob"
           """|]
        [str "Hi,\n \"Bob\"\n"]
      parseOk
        [r|"""
           """|]
        [str ""]
      parseOk
        [r|"""
           "
           """|]
        [str "\"\n"]
      parseOk
        [r|"""
           \\
           """|]
        [str "\\\n"]
      parseOk
        [r|"""
            "When I use a word," Humpty Dumpty said,
            in rather a scornful tone, "it means just what I
            choose it to mean - neither more nor less."
            "The question is," said Alice, "whether you
            can make words mean so many different things."
            "The question is," said Humpty Dumpty,
            "which is to be master - that's all."
        """|]
        [ str
            "    \"When I use a word,\" Humpty Dumpty said,\n\
            \    in rather a scornful tone, \"it means just what I\n\
            \    choose it to mean - neither more nor less.\"\n\
            \    \"The question is,\" said Alice, \"whether you\n\
            \    can make words mean so many different things.\"\n\
            \    \"The question is,\" said Humpty Dumpty,\n\
            \    \"which is to be master - that's all.\"\n\
            \"
        ]
      parseFail
        [r|"""
            "When I use a word," Humpty Dumpty said,
            in rather a scornful tone, "it means just what I
            choose it to mean - neither more nor less."
            "The question is," said Alice, "whether you
            can make words mean so many different things."
            "The question is," said Humpty Dumpty,
            "which is to be master - that's all.""""|]
      parseOk
        [r|"""
            "When I use a word," Humpty Dumpty said,
            in rather a scornful tone, "it means just what I
            choose it to mean - neither more nor less."
            "The question is," said Alice, "whether you
            can make words mean so many different things."
            "The question is," said Humpty Dumpty,
            "which is to be master - that's all.\""""|]
        [ str
            "\"When I use a word,\" Humpty Dumpty said,\n\
            \in rather a scornful tone, \"it means just what I\n\
            \choose it to mean - neither more nor less.\"\n\
            \\"The question is,\" said Alice, \"whether you\n\
            \can make words mean so many different things.\"\n\
            \\"The question is,\" said Humpty Dumpty,\n\
            \\"which is to be master - that's all.\""
        ]
      parseOk
        [r|"""
            String text = \"""
                The quick brown fox jumps over the lazy dog
            \""";
            """|]
        [str "String text = \"\"\"\n\
             \    The quick brown fox jumps over the lazy dog\n\
             \\"\"\";\n"
            ]
      {-
        TODO: the following case doesn't pass, but Java spec isn't very clear on how to handle line continuation.
         expected: Right [StringLiteral "   aaa\n     bbb  ccc\n     def\n    g\n"]
          but got: Right [StringLiteral "   aaa\n     bbb          ccc\n     def\n    g\n"]
         plan:
         - first scan treating line continuation as regular \n, and figure out indent x.
         - second scan discard x whitespaces after line continuation.
         - stripIndent.
       -}
      {-
      parseOk
        [r|"""
           aaa
             bbb\
          ccc
             def
            g
        """|]
        [str "   aaa\n\
             \     bbb  ccc\n\
             \     def\n\
             \    g\n\
             \"] -}

