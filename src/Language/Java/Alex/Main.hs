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
