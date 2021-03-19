module Language.Java.Alex.Main
  ( main
  )
where

import Data.Char

javaKeywords :: [String]
javaKeywords =
  [ "abstract"
  , "continue"
  , "for"
  , "new"
  , "switch"
  , "assert"
  , "default"
  , "if"
  , "package"
  , "synchronized"
  , "boolean"
  , "do"
  , "goto"
  , "private"
  , "this"
  , "break"
  , "double"
  , "implements"
  , "protected"
  , "throw"
  , "byte"
  , "else"
  , "import"
  , "public"
  , "throws"
  , "case"
  , "enum"
  , "instanceof"
  , "return"
  , "transient"
  , "catch"
  , "extends"
  , "int"
  , "short"
  , "try"
  , "char"
  , "final"
  , "interface"
  , "static"
  , "void"
  , "class"
  , "finally"
  , "long"
  , "strictfp"
  , "volatile"
  , "const"
  , "float"
  , "native"
  , "super"
  , "while"
  , "_"
  ]

toTokenAlt :: String -> String
toTokenAlt = toTok . tr
  where
    tr "_" = "symbolUnderscore"
    tr x = x
    toTok x = "Kw" <> [toUpper (head x)] <> tail x

_mainGenToken :: IO ()
_mainGenToken = mapM_ (putStrLn . ("  | " <>) . toTokenAlt) javaKeywords

main :: IO ()
main = mapM_ go javaKeywords
  where
    go kwRaw = putStrLn $ " , (" <> show kwRaw <> ", " <> toTokenAlt kwRaw <> ")"

{-
  Latest spec seems to be:
  - https://docs.oracle.com/javase/specs/jls/se16/html/index.html
  - https://docs.oracle.com/javase/specs/jls/se16/html/jls-3.html
 -}

{-
  3.2. Lexical Translations

  TODO:
  For now I'm not sure how to deal with consecutive '>' characters properly,
  as to be able to tell a context means we at least the AST available,
  which will require lexical analysis to be done, which we are currently doing.

 -}

{-
  We are going with String-based lexer.

  This makes it easy to be compliant with the spec,
  this is because we can pre-process the String before
  passing it to Alex, to handle following cases properly:

  - perform the initial Unicode scan as specified.
  - normalize CR+LF / CR / LF (here I choose to normalize to LF)

  TODO: "3.5. Input Elements and Tokens" mentions about a potential control-z at the end,
  how should we deal with it?

 -}

{-
  Identifiers:

  A "Java letter" is a character for which the method Character.isJavaIdentifierStart(int) returns true.
  A "Java letter-or-digit" is a character for which the method Character.isJavaIdentifierPart(int) returns true.

 -}
