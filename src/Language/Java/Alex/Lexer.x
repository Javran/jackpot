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

@Digits = $digit|($digit($digit|_)*$digit)

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
  (0|((1-9)(@Digits)?)|((1-9)_+@Digits))[lL]?
    -- DecimalIntegerLiteral
    { \(_, ch, xs, _) l -> getHsInteger ch xs l }
  0[xX]($hexdigit|$hexdigit($hexdigit|_)*$hexdigit)[lL]?
    -- HexIntegerLiteral
    { \(_, ch, xs, _) l -> getHsInteger ch xs l }
  0(0-7|0-7([_0-7])*0-7)[lL]?
    -- OctalIntegerLiteral
    { \(_, ch, xs, _) l -> getOctal ch xs l }
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
