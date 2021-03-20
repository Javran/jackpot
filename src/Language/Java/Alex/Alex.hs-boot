module Language.Java.Alex.Alex where

import Data.Word (Word8)

type Byte = Word8

type AlexInput =
  ( AlexPosn
  , Char
  , [Byte]
  , String
  )

type AlexAction result = AlexInput -> Int -> Alex result

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexInputPrevChar :: AlexInput -> Char

data AlexPosn

data AlexError

alexError :: String -> Alex a

newtype Alex a = Alex {unAlex :: AlexState -> Either AlexError (a, AlexState)}

data AlexState = AlexState
  { alex_pos :: !AlexPosn
  , alex_inp :: String
  , alex_chr :: !Char
  , alex_bytes :: [Byte]
  , alex_scd :: !Int
  }

instance Applicative Alex

instance Monad Alex
