module Language.Java.Lexical.PlatformFunction where

import Data.Char (GeneralCategory (..), isSpace)
import Data.Char.GeneralCategory.V13_0_0
import Data.Ix
import Data.List
import Data.List.Split

{-
  The goal of this module is to implement a counterpart of following functions:

  - Character.isJavaIdentifierStart(int)
  - Character.isJavaIdentifierPart(int)
  - String.stripIndent

  as required by Java spec.
 -}

{-
  A character may start a Java identifier if and only if one of the following conditions is true:

  + isLetter(codePoint) returns true
  + getType(codePoint) returns LETTER_NUMBER
  + the referenced character is a currency symbol (such as '$')
  + the referenced character is a connecting punctuation character (such as '_').
 -}
isJavaIdentifierStart :: Char -> Bool
isJavaIdentifierStart ch =
  isLetter ch || gc
    `elem` [ LetterNumber
           , CurrencySymbol
           , ConnectorPunctuation
           ]
  where
    gc = generalCategory ch

{-
  A character may be part of a Java identifier if any of the following are true:

  + it is a letter
  + it is a currency symbol (such as '$')
  + it is a connecting punctuation character (such as '_')
  + it is a digit
  + it is a numeric letter (such as a Roman numeral character)
  + it is a combining mark
  + it is a non-spacing mark
  + isIdentifierIgnorable returns true for the character

 -}
isJavaIdentifierPart :: Char -> Bool
isJavaIdentifierPart ch =
  isLetter ch
    || (
        {-
          I'm not actually sure why OtherNumber category is excluded,
          but Java's counterpart is implemented this way.
         -}
        gc /= OtherNumber)
      && isNumber ch
    || gc
    `elem` [ CurrencySymbol
           , ConnectorPunctuation
           , SpacingCombiningMark
           , NonSpacingMark
           ]
    || isIdentifierIgnorable ch
  where
    gc = generalCategory ch

{-
  The following Unicode characters are ignorable in a Java identifier or a Unicode identifier:

  + ISO control characters that are not whitespace
  + '\u0000' through '\u0008'
  + '\u000E' through '\u001B'
  + '\u007F' through '\u009F'
  + all characters that have the FORMAT general category value
 -}
isIdentifierIgnorable :: Char -> Bool
isIdentifierIgnorable ch =
  inRange ('\x0000', '\x0008') ch
    || inRange ('\x000E', '\x0001B') ch
    || inRange ('\x007F', '\x009F') ch
    || gc == Format
  where
    gc = generalCategory ch

stripIndent :: String -> String
stripIndent raw = result
  where
    {-
      First, the individual lines of this string are extracted.
      note that `indivLines` is always non-empty.
     -}
    indivLines = splitOn "\n" raw
    pairs :: [(String, String)]
    pairs = fmap (span isSpace) indivLines
    {-
      Then, the minimum indentation (min) is determined as follows:
      + For each non-blank line (as defined by isBlank()),
        the leading white space characters are counted.
      + The leading white space characters on the last line are also counted even if blank.
      The min value is the smallest of these counts.
     -}
    minIndent =
      minimum $
        fmap (length . fst) $
          -- last line always counts
          last pairs :
          -- otherwise only non-blank lines count
          filter (not . null . snd) (init pairs)
    {-
      For each non-blank line, min leading white space characters are removed,
      and any trailing white space characters are removed.
      Blank lines are replaced with the empty string.
     -}
    resultLines = fmap (dropWhileEnd isSpace . drop minIndent) indivLines

    {-
      Finally, the lines are joined into a new string, using the LF character "\n" (U+000A) to separate lines.
     -}
    result = intercalate "\n" resultLines
