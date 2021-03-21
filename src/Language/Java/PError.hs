module Language.Java.PError where

import Text.Megaparsec

{-
  This funny name so I don't have to hide ParseError from megaparsec.
 -}

data PError
  = SimpleError {ePpr :: String}
  | UnicodeEscapeError {ePpr :: String}
  deriving (Show, Eq, Ord)

instance ShowErrorComponent PError where
  showErrorComponent = show
