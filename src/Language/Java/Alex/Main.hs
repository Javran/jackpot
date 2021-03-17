module Language.Java.Alex.Main
  ( main
  )
where

main :: IO ()
main = pure ()

{-
  Latest spec seems to be:
  - https://docs.oracle.com/javase/specs/jls/se16/html/index.html
  - https://docs.oracle.com/javase/specs/jls/se16/html/jls-3.html
 -}

{-
  3.4. Line Terminators

  TODO: For simplicity we'll just keep track of line numbers
  supplied by Alex - this is not compliant with the spec
  but for now I don't think it matters.

  Multiple workaround ideas:
  - keep track by a user state
  - parse newline into a token
  - do a first round of "scan" to normalize it.

 -}

{-
  3.2. Lexical Translations

  1. TODO: for simplicity no Unicode character for now.
  2. & 3. : those two steps can probably be combined into one?

 -}

{-
  TODO: we should probably go with String-based lexer.

  This will makes it easy to be compliant with the spec,
  this is because we can pre-process the String before
  passing it to Alex, to handle following cases properly:

  - CR+LF / CR / LF to be normalized.
  - perform the initial Unicode scan as specified.

 -}
