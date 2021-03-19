{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.Java.Alex.Lexer where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as BSL
import Language.Java.Alex.Token

}

%wrapper "monad"

$digit = 0-9
$hexdigit = [0-9a-fA-F]
$alpha = [a-zA-Z]

-- WhiteSpace in Java is:
-- SP (\ ) HT (\t) FF (\f)
-- CR (\r) LF (\n) but $white includes VT (\v) and we need to exclude that.
$JavaWhiteSpace = $white # \v

-- Note that Alex excludes `\n` by default, so we have to be explicit about it.
$NotStar = [.\n] # \*
$NotStarNotSlash = $NotStar # \/

-- 3.1. Unicode mentions that:
-- > Except for comments (§3.7), identifiers (§3.8, and the contents of character literals,
-- > string literals, and text blocks (§3.10.4, §3.10.5, §3.10.6),
-- > all input elements (§3.5) in a program are formed only from ASCII characters
-- > (or Unicode escapes (§3.3) which result in ASCII characters).
-- So here the goal for Lexer is to just handle range \x00-\x7F properly and rely on Alex action
-- to handle this properly.
$JavaIdentifierStartLite = [\x24\x41-\x5A\x5F\x61-\x7A\x80-\x10ffff]
$JavaIdentifierPartLite = [$JavaIdentifierStartLite\x00-\x08\x0E-\x1B\x30-\x39\x7F\x80-x10ffff]

-- Characters that could be part of a number literal
-- Note that this is intentionally boarder than the actual language.
-- This allows us to handle actions with more informative error messages.
$NumLitPart = [0-9A-Za-z_]

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

  $JavaWhiteSpace+
    ;

  -- EndOfLineComment
  "//".*
    ;
  -- TraditionalComment
  -- This is a tricky bit, the FSA from spec is actually easy to write:
  --
  --                       [^\*\/]
  --                      /        \
  --                     /          \
  --                    v            ^
  -- [start] --"/*"--> [0] --"*"--> [1] --"/"--> [accept]
  --                  ^   v        ^   v
  --                 /     \      /     \
  --                 \     /      \     /
  --                  [^\*]         "*"
  -- Now the problem is how to translate this back to regex.
  -- First let's get rid of two loopy bits, which we can add back later:
  --
  --                       [^\*\/]
  --                      /        \
  --                     /          \
  --                    v            ^
  -- [start] --"/*"--> [0] --"*"--> [1] --"/"--> [accept]
  --
  -- Note that if we expand this a bit:
  -- + "/*" "*" "/" [accepted]
  -- + "/*" "*" [^\*\/] "*" "/" [accepted]
  -- + "/*" "*" [^\*\/] "*" [^\*\/] "*" "/" [accepted]
  -- + ...
  --
  -- Spot the pattern: "/*" "*" ([^\*\/] "*")* "/"
  -- Now if we add back loops that we ignored earlier, we get the final product below:
  "/*"$NotStar*\*(\**$NotStarNotSlash$NotStar*\*(\*)*|\*)*\/
    ;
  true
    { \_ _ -> pure (BooleanLiteral True) }
  false
    { \_ _ -> pure (BooleanLiteral False) }
  null
    { \_ _ -> pure NullLiteral }

  -- IntegerLiteral
  --   DecimalIntegerLiteral
  --     note that the regex below is not yet ready to be merged with others
  --     as it would interfere with floating point literals.
  [0-9][_0-9]*@IntegerTypeSuffix?
    { \(_, ch, _, xs) l -> getIntegerLiteral @Alex ch (take l xs) }
  --   HexIntegerLiteral
  --   OctalIntegerLiteral
  --   BinaryIntegerLiteral
  0[bBxX]?$NumLitPart+@IntegerTypeSuffix?
    { \(_, ch, _, xs) l -> getIntegerLiteral @Alex ch (take l xs) }

  -- FloatingPointLiteral
  --   DecimalFloatingPointLiteral
  @Digits\.@Digits?@ExponentPart?@FloatTypeSuffix?
    { \(_, ch, _, xs) l -> getFloatingPoint @Alex ch xs l }
  --   DecimalFloatingPointLiteral
  \.@Digits@ExponentPart?@FloatTypeSuffix?
    { \(_, ch, _, xs) l -> getFloatingPoint @Alex ch xs l }
  --   DecimalFloatingPointLiteral
  @Digits@ExponentPart@FloatTypeSuffix?
    { \(_, ch, _, xs) l -> getFloatingPoint @Alex ch xs l }
  --   DecimalFloatingPointLiteral
  @Digits@ExponentPart?@FloatTypeSuffix
    { \(_, ch, _, xs) l -> getFloatingPoint @Alex ch xs l }
  --  HexadecimalFloatingPointLiteral
  @HexSignificand@BinaryExponent@FloatTypeSuffix?
    { \(_, ch, _, xs) l -> getFloatingPoint @Alex ch xs l }

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
