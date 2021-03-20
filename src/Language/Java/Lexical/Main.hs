{-# LANGUAGE LambdaCase #-}

module Language.Java.Lexical.Main
  ( main
  )
where

import Language.Java.Lexical.Wrapper
import System.Environment

main :: IO ()
main =
  getArgs >>= \case
    [fp] -> do
      raw <- readFile fp
      print $ parseAll raw
    _ -> error "unsupported"

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

 -}
