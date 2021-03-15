module Language.Java.Alex.Token where

data Token
  = Todo -- TODO: impl other tokens
  | BooleanLiteral Bool
  | IntegerLiteral Integer Bool {- whether IntegerTypeSuffix is present -}
  | NullLiteral
  | EndOfFile
  deriving (Eq, Show)
