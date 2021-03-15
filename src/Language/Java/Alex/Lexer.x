{
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Java.Alex.Lexer where

import Language.Java.Alex.Token

}

%wrapper "monad-bytestring"

$digit = 0-9
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
  (0|((1-9)(@Digits)?)|((1-9)_+@Digits))([lL])?
    -- DecimalIntegerLiteral
    { \(_, _, xs, _) l -> pure Todo }

{

alexEOF = pure EndOfFile

}
