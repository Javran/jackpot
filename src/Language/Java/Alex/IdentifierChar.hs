{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Java.Alex.IdentifierChar where

import Data.Char
import Data.FileEmbed
import qualified Data.IntSet as IS
import Data.Ix


{-
  The goal of this module is to implement a counterpart of following functions:

 - Character.isJavaIdentifierStart(int)
 - Character.isJavaIdentifierPart(int)

  as required by Java spec.

  TODO: for now Java is using Unicode 13 while GHC's UnicodeData is a bit outdated,
  and this is making differences in generalCategory.
  We'll need to somehow keep track of Unicode 13 data - before that we cannot follow
  the description just by using counterpart functions in Haskell.

 -}

javaIdentifierStarts :: IS.IntSet
javaIdentifierStarts = IS.fromList $ read @[Int] $(embedStringFile "embed-data/start.txt")

isJavaIdentifierStart :: Char -> Bool
isJavaIdentifierStart ch = ord ch `IS.member` javaIdentifierStarts

javaIdentifierParts :: IS.IntSet
javaIdentifierParts = IS.fromList $ read @[Int] $(embedStringFile "embed-data/part.txt")

isJavaIdentifierPart :: Char -> Bool
isJavaIdentifierPart ch = ord ch `IS.member` javaIdentifierParts

{-
  A character may start a Java identifier if and only if one of the following conditions is true:

  + isLetter(codePoint) returns true
  + getType(codePoint) returns LETTER_NUMBER
  + the referenced character is a currency symbol (such as '$')
  + the referenced character is a connecting punctuation character (such as '_').
 -}
_isJavaIdentifierStart :: Char -> Bool
_isJavaIdentifierStart ch =
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
_isJavaIdentifierPart :: Char -> Bool
_isJavaIdentifierPart ch =
  isLetter ch
    || isNumber ch
    || gc
    `elem` [ CurrencySymbol
           , ConnectorPunctuation
           , SpacingCombiningMark
           , NonSpacingMark
           ]
    || _isIdentifierIgnorable ch
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
_isIdentifierIgnorable :: Char -> Bool
_isIdentifierIgnorable ch =
  inRange ('\x0000', '\x0008') ch
    || inRange ('\x000E', '\x0001B') ch
    || inRange ('\x007F', '\x009F') ch
    || gc == Format
  where
    gc = generalCategory ch
