{-# LANGUAGE FlexibleContexts #-}

module Language.Java.Alex.Token where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Char
import Data.Int
import Data.Scientific
import Language.Java.Alex.FloatingPoint (floatingPointLiteralS)
import Numeric

data Token
  = Todo BSL.ByteString
  | BooleanLiteral Bool
  | IntegerLiteral Integer Bool {- whether IntegerTypeSuffix is present -}
  | FloatingPointLiteral Scientific Bool {- whether this is double (True) or float (False) -}
  | NullLiteral
  | EndOfFile
  deriving (Eq, Show)

todo :: Applicative f => (a, b, BSLC.ByteString, d) -> Int64 -> f Token
todo (_, _, xs, _) l = pure $ Todo (BSLC.take l xs)

{-
  TODO: To be compliant with Java spec
  we need to rule out out-of-bound integers at this stage,
  however it won't be as straightforward due to the interaction with
  prefixing "-" unary operation.

  TODO: probably the problem lies in the fact that we don't have
  a distinction on whether an error is recoverable - if we are parsing an int,
  and gets an error because the integer doesn't fit or there are invalid digits,
  we should generate an error that is not recoverable rather than proceeding to try other things.

 -}

parseByRead :: MonadError String m => ReadS Integer -> Char -> String -> m Token
parseByRead _ prevChar _
  | isDigit prevChar =
    {-
     This is weird case is necessary to handle parsing octal literals correctly:

     Without special handling, things like "08" or "0123489", which is supposed to be
     invalid octals are tokenized as two literals: (0 and then 8) or (01234 and then 89).
     To prevent this from happening, decimal tokenizer should check whether previous char is a digit
     and reject if that is indeed the case.
    -}
    throwError "integer literal cannot directly follow any digit"
parseByRead reader _ inp =
  case reader $ filter (/= '_') inp of
    [(v, mayL)]
      | mayL `elem` ["", "L", "l"] ->
        pure $ IntegerLiteral v (mayL /= "")
    _ -> throwError "parse error"

{-
  Side note: what is the previous char even before Alex start to consume anything?

  Answer: '\n', according to:
  https://github.com/simonmar/alex/blob/35f07a1c272c6b3aace858c2b1b0c427a1d89e67/data/AlexWrappers.hs#L201
 -}
getDecimalOrHex :: MonadError String m => Char -> BSL.ByteString -> Int64 -> m Token
getDecimalOrHex prevCh xs l =
  parseByRead reads prevCh inp
  where
    inp = BSLC.unpack $ BSLC.take l xs

getOctal :: MonadError String m => Char -> BSL.ByteString -> Int64 -> m Token
getOctal prevCh xs l =
  parseByRead reads prevCh ("0o" <> filter (/= '_') inp)
  where
    '0' : inp = BSLC.unpack $ BSLC.take l xs

getBinary :: MonadError String m => Char -> BSL.ByteString -> Int64 -> m Token
getBinary prevCh xs l = parseByRead reader prevCh inp
  where
    reader = readInt 2 (`elem` "01") (\ch -> ord ch - ord '0')
    '0' : _b : inp = BSLC.unpack $ BSLC.take l xs

getFloatingPoint :: MonadError String m => Char -> BSL.ByteString -> Int64 -> m Token
getFloatingPoint prevChar _ _
  | isDigit prevChar =
    throwError "floating point literal cannot directly follow any digit"
getFloatingPoint _ xs l = case floatingPointLiteralS inp of
  [((s, d), "")] -> pure $ FloatingPointLiteral s d
  _ -> throwError "parse error"
  where
    inp = BSLC.unpack $ BSLC.take l xs
