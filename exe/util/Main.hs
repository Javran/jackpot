{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import System.Environment
import Text.Replace

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

mainGenToken :: IO ()
mainGenToken = mapM_ (putStrLn . ("  | " <>) . toTokenAlt) javaKeywords

mainGenKeywords :: IO ()
mainGenKeywords = mapM_ go javaKeywords
  where
    go kwRaw = putStrLn $ " , (" <> show kwRaw <> ", " <> toTokenAlt kwRaw <> ")"

mainGenRegex :: IO ()
mainGenRegex = do
  putStrLn "TextBlock:"
  putStr "  "
  putStrLn $
    replaceWithList
      [ Replace "q" "\\\""
      , Replace "s" "\\\\"
      , Replace "z" "$NotDbQuoteNotSlash"
      , Replace "+" "|"
      ]
      "qqq(z+s(q+s+z)+q(z+s(q+s+z)+q(z+s(q+s+z))))*qqq"
  putStrLn "TraditionalComment:"
  putStr "  "
  putStrLn $
    replaceWithList
      [ Replace "a" "\\/"
      , Replace "b" "\\*"
      , Replace "c" "$NotStarNotSlash"
      , Replace "+" "|"
      ]
      "ab(c+a+bb*c)*bb*a"

main :: IO ()
main =
  getArgs >>= \case
    "tokenalt" : _ -> mainGenToken
    "kw" : _ -> mainGenKeywords
    "regex" : _ -> mainGenRegex
    _ -> error "unsupported command"
