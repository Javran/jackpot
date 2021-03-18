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

  - https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html#isJavaIdentifierStart(int)

    A character may start a Java identifier if and only if one of the following conditions is true:

    + isLetter(codePoint) returns true
    + getType(codePoint) returns LETTER_NUMBER
    + the referenced character is a currency symbol (such as '$')
    + the referenced character is a connecting punctuation character (such as '_').

    A character is considered to be a letter if its general category type, provided by getType(codePoint), is any of the following:

    + UPPERCASE_LETTER
    + LOWERCASE_LETTER
    + TITLECASE_LETTER
    + MODIFIER_LETTER
    + OTHER_LETTER

  - https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html#isJavaIdentifierPart(int)

    A character may be part of a Java identifier if any of the following are true:

    + it is a letter
    + it is a currency symbol (such as '$')
    + it is a connecting punctuation character (such as '_')
    + it is a digit
    + it is a numeric letter (such as a Roman numeral character)
    + it is a combining mark
    + it is a non-spacing mark
    + isIdentifierIgnorable returns true for the character

    isIdentifierIgnorable:

    The following Unicode characters are ignorable in a Java identifier or a Unicode identifier:

    + ISO control characters that are not whitespace
    + '\u0000' through '\u0008'
    + '\u000E' through '\u001B'
    + '\u007F' through '\u009F'
    + all characters that have the FORMAT general category value

  Following results by manual evaluation: (a group is a consecutive range)

  isJavaIdentifierStart:
    Total 125393, groups: 606

  isJavaIdentifierPart:
    Total 128354, groups: 717

  Note that from section 3.1 Unicode:

  > Except for comments (§3.7), identifiers (§3.8, and the contents of character literals, string literals, and text blocks (§3.10.4, §3.10.5, §3.10.6), all input elements (§3.5) in a program are formed only from ASCII characters (or Unicode escapes (§3.3) which result in ASCII characters).

  This provides a potential alternative that does not require printing out the full set in Lexer.x

 -}
