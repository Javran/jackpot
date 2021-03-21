module Language.Java.PError where

{-
  This funny name so I don't have to hide ParseError from megaparsec.
 -}

data PError
  = UnicodeEscapeError
  | AlexSimpleError String
  deriving (Show, Eq, Ord)
