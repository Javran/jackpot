{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.Java.Alex.Lexer where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as BSL
import Language.Java.Alex.Token

}

%wrapper "monad-bytestring"

$digit = 0-9
$hexdigit = [0-9a-fA-F]
$alpha = [a-zA-Z]

@Digits = $digit|$digit($digit|_)*$digit
@IntegerTypeSuffix = [lL]
@HexDigits = $hexdigit|$hexdigit($hexdigit|_)*$hexdigit
@HexNumeral = 0[xX]@HexDigits
@SignedInteger = [\+\-]?@Digits
@ExponentPart = [eE][\+\-]?@SignedInteger
@FloatTypeSuffix = [fFdD]
@HexSignificand = @HexNumeral\.?|0[xX]@HexDigits?\.@HexDigits
@BinaryExponent = [pP]@SignedInteger

tokens :-

  $white+
    ;
  "//".*
    ;
  true
    { \_ _ -> pure (BooleanLiteral True) }
  false
    { \_ _ -> pure (BooleanLiteral False) }
  null
    { \_ _ -> pure NullLiteral }

  -- IntegerLiteral
  --   DecimalIntegerLiteral
  (0|(1-9)@Digits?|(1-9)_+@Digits)@IntegerTypeSuffix?
    { \(_, ch, xs, _) l -> getDecimalOrHex ch xs l }
  --   HexIntegerLiteral
  @HexNumeral@IntegerTypeSuffix?
    { \(_, ch, xs, _) l -> getDecimalOrHex ch xs l }
  --   OctalIntegerLiteral
  0(0-7|0-7([_0-7])*0-7)@IntegerTypeSuffix?
    { \(_, ch, xs, _) l -> getOctal ch xs l }
  --   BinaryIntegerLiteral
  0[bB](0-1|0-1([_0-1])*0-1)@IntegerTypeSuffix?
    { \(_, ch, xs, _) l -> getBinary ch xs l }

  -- FloatingPointLiteral
  --   DecimalFloatingPointLiteral
  @Digits\.@Digits?@ExponentPart?@FloatTypeSuffix?
    { todo }
  --   DecimalFloatingPointLiteral
  \.@Digits@ExponentPart?@FloatTypeSuffix?
    { todo }
  --   DecimalFloatingPointLiteral
  @Digits@ExponentPart@FloatTypeSuffix?
    { todo }
  --   DecimalFloatingPointLiteral
  @Digits@ExponentPart?@FloatTypeSuffix
    { todo }
  --  HexadecimalFloatingPointLiteral
  @HexSignificand@BinaryExponent@FloatTypeSuffix?
    { \(_, ch, xs, _) l -> getFloatingPoint ch xs l }

{

alexEOF :: Alex Token
alexEOF = pure EndOfFile

instance MonadError String Alex where
  throwError e = Alex (const (Left e))
  catchError m handler = Alex $ \st -> case unAlex m st of
    Left e -> unAlex (handler e) st
    Right v -> Right v

instance MonadState AlexState Alex where
  state f = Alex $ \s -> let (v, s') = f s in pure (s', v)

}
