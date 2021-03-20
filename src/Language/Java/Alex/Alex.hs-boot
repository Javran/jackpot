module Language.Java.Alex.Alex where

import Data.Word (Word8)

type Byte = Word8

type AlexInput =
  ( AlexPosn -- current position,
  , Char -- previous char
  , [Byte] -- pending bytes on current char
  , String -- current input string
  )

type AlexAction result = AlexInput -> Int -> Alex result

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexInputPrevChar :: AlexInput -> Char

data AlexPosn = AlexPn !Int !Int !Int

newtype Alex a = Alex {unAlex :: AlexState -> Either String (AlexState, a)}

data AlexState = AlexState
  { alex_pos :: !AlexPosn -- position at current input location
  , alex_inp :: String -- the current input
  , alex_chr :: !Char -- the character before the input
  , alex_bytes :: [Byte]
  , alex_scd :: !Int -- the current startcode
  }

instance Applicative Alex

instance Monad Alex
