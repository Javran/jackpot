module Language.Java.PError where

{-
  This funny name so I don't have to hide ParseError from megaparsec.
 -}

data PError
  = SimpleError {ePpr :: String}
  | UnicodeEscapeError {ePpr :: String}
  deriving (Show, Eq, Ord)
