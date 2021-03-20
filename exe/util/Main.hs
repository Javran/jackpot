{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Bifunctor
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
mainGenKeywords =
  forM_ javaKeywords $ \kwRaw -> do
    putStrLn $ "  \"" <> kwRaw <> "\""
    putStrLn $ "    { mkTokConst " <> toTokenAlt kwRaw <> " }"

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

separators :: [(String, String)]
separators =
  (fmap . second)
    ("Sep" <>)
    [ ("(", "LParen")
    , (")", "RParen")
    , ("{", "LBrace")
    , ("}", "RBrace")
    , ("[", "LBracket")
    , ("]", "RBracket")
    , (";", "SColon")
    , (",", "Comma")
    , (".", "Dot")
    , ("...", "TripleDot")
    , ("@", "At")
    , ("::", "DbColon")
    ]

operators :: [(String, String)]
operators =
  (fmap . second)
    ("Op" <>)
    [ ("=", "Eq")
    , (">", "Gt")
    , ("<", "Lt")
    , ("!", "Exclam")
    , ("~", "Tilde")
    , ("?", "Que")
    , (":", "Col")
    , ("->", "MinusGt")
    , ("==", "EqEq")
    , ("<=", "Le")
    , ("!=", "Ne")
    , ("&&", "AndAnd")
    , ("||", "OrOr")
    , ("++", "PlusPlus")
    , ("--", "MinusMinus")
    , ("+", "Plus")
    , ("-", "Minus")
    , ("*", "Star")
    , ("/", "Slash")
    , ("&", "And")
    , ("|", "Or")
    , ("^", "Caret")
    , ("%", "Percent")
    , ("<<", "LtLt")
    , ("+=", "PlusEq")
    , ("-=", "MinusEq")
    , ("*=", "StarEq")
    , ("/=", "SlashEq")
    , ("&=", "AndEq")
    , ("|=", "OrEq")
    , ("^=", "CaretEq")
    , ("%=", "PercentEq")
    , ("<<=", "LtLtEq")
    -- , (">=", "")
    -- , (">>", "")
    -- , (">>>", "")
    -- , (">>=", "")
    -- , (">>>=", "")
    ]

mainOpSep :: IO ()
mainOpSep = do
  putStrLn "Alts:"
  forM_ (separators <> operators) $ \(_lit, tok) -> do
    putStrLn $ "  | " <> tok
  putStrLn "Lexer:"
  forM_ (separators <> operators) $ \(lit, tok) -> do
    putStrLn $ "  \"" <> lit <> "\""
    putStrLn $ "    { mkTokConst " <> tok <> " }"

main :: IO ()
main =
  getArgs >>= \case
    "tokenalt" : _ ->
      -- token data alternatives
      mainGenToken
    "kw" : _ ->
      -- (keyword str, data type) pairs
      mainGenKeywords
    "regex" : _ ->
      -- those two complicated regexes.
      mainGenRegex
    "opsep" : _ ->
      -- gen operator and separator related stuff
      mainOpSep
    _ -> error "unsupported command"
