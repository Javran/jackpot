{-# LANGUAGE FlexibleContexts #-}

module Language.Java.Alex.Token where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
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

getHsInteger :: MonadError String m => BSL.ByteString -> Int64 -> m Token
getHsInteger xs l =
  case reads (filter (/= '_') inp) of
    [(v, mayL)]
      | mayL `elem` ["", "L", "l"] ->
        pure $ IntegerLiteral v (mayL /= "")
    _ -> throwError "parse error"
  where
    inp = BSLC.unpack $ BSLC.take l xs

