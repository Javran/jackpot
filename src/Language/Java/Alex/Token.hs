{-# LANGUAGE FlexibleContexts #-}

module Language.Java.Alex.Token where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Char
import Data.Int

data Token
  = Todo BSL.ByteString
  | BooleanLiteral Bool
  | IntegerLiteral Integer Bool {- whether IntegerTypeSuffix is present -}
  | NullLiteral
  | EndOfFile
  deriving (Eq, Show)

{-
  TODO: To be compliant with Java spec
  we need to rule out out-of-bound integers at this stage,
  however it won't be as straightforward due to the interaction with
  prefixing "-" unary operation.
 -}

{-
  Side note: what is the previous char even before Alex start to consume anything?

  Answer: '\n', according to:
  https://github.com/simonmar/alex/blob/35f07a1c272c6b3aace858c2b1b0c427a1d89e67/data/AlexWrappers.hs#L201
 -}
getHsInteger :: MonadError String m => Char -> BSL.ByteString -> Int64 -> m Token
getHsInteger ch _ _
  | isDigit ch =
    {-
     This is weird case is necessary to handle parsing octal literals correctly:

     Without special handling, things like "08" or "0123489", which is supposed to be
     invalid octals are tokenized as two literals: (0 and then 8) or (01234 and then 89).
     To prevent this from happening, decimal tokenizer should check whether previous char is a digit
     and reject if that is indeed the case.
    -}
    throwError "integer literal cannot directly follow any digit"
getHsInteger _ xs l =
  case reads (filter (/= '_') inp) of
    [(v, mayL)]
      | mayL `elem` ["", "L", "l"] ->
        pure $ IntegerLiteral v (mayL /= "")
    _ -> throwError "parse error"
  where
    inp = BSLC.unpack $ BSLC.take l xs

-- TODO: refactor
getOctal :: MonadError String m => Char -> BSL.ByteString -> Int64 -> m Token
getOctal _ xs l =
  case reads ("0o" <> filter (/= '_') inp) of
    [(v, mayL)]
      | mayL `elem` ["", "L", "l"] ->
        pure $ IntegerLiteral v (mayL /= "")
    _ -> throwError "parse error"
  where
    '0' : inp = BSLC.unpack $ BSLC.take l xs
