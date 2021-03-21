{-# LANGUAGE ConstraintKinds #-}

module Language.Java.Lexical.Alex where

import Control.Effect.Error
import Control.Effect.State
import Data.Word (Word8)
import Language.Java.PError

type Byte = Word8

type AlexInput =
  ( AlexPosn
  , Char
  , [Byte]
  , String
  )

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexInputPrevChar :: AlexInput -> Char

data AlexPosn

alexError :: Alex sig m => String -> m a

type Alex sig m =
  ( Has (State AlexState) sig m
  , Has (Error PError) sig m
  )

data AlexState