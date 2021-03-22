{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.Java.Lexical.Lexer where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as BSL
import Language.Java.Lexical.Token
import {-# SOURCE #-} Language.Java.Lexical.Alex
}

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
$NotDbQuoteNotSlash = [.\n] # \" # \/

-- See: [Regarding $JavaIdentifierStart and $JavaIdentifierPart] section in /docs/README.md
$JavaIdentifierStartLite = [\x24\x41-\x5A\x5F\x61-\x7A\x80-\x10ffff]
$JavaIdentifierPartLite = [$JavaIdentifierStartLite\x00-\x08\x0E-\x1B\x30-\x39\x7F\x80-\x10ffff]

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

-- loose match, relies on Alex action.
@EscapeBody = \\(.|[0-9]+)

tokens :-

  $JavaWhiteSpace+
    ;

  -- EndOfLineComment
  "//".*
    ;
  -- TraditionalComment
  -- See: [Regular expressions] section in /docs/README.md
  \/\*($NotStarNotSlash|\/|\*\**$NotStarNotSlash)*\*\**\/
    ;
  "true"
    { mkTokConst (BooleanLiteral True) }
  "false"
    { mkTokConst (BooleanLiteral False) }
  "null"
    { mkTokConst NullLiteral }

  -- Keywords
  "abstract"
    { mkTokConst KwAbstract }
  "continue"
    { mkTokConst KwContinue }
  "for"
    { mkTokConst KwFor }
  "new"
    { mkTokConst KwNew }
  "switch"
    { mkTokConst KwSwitch }
  "assert"
    { mkTokConst KwAssert }
  "default"
    { mkTokConst KwDefault }
  "if"
    { mkTokConst KwIf }
  "package"
    { mkTokConst KwPackage }
  "synchronized"
    { mkTokConst KwSynchronized }
  "boolean"
    { mkTokConst KwBoolean }
  "do"
    { mkTokConst KwDo }
  "goto"
    { mkTokConst KwGoto }
  "private"
    { mkTokConst KwPrivate }
  "this"
    { mkTokConst KwThis }
  "break"
    { mkTokConst KwBreak }
  "double"
    { mkTokConst KwDouble }
  "implements"
    { mkTokConst KwImplements }
  "protected"
    { mkTokConst KwProtected }
  "throw"
    { mkTokConst KwThrow }
  "byte"
    { mkTokConst KwByte }
  "else"
    { mkTokConst KwElse }
  "import"
    { mkTokConst KwImport }
  "public"
    { mkTokConst KwPublic }
  "throws"
    { mkTokConst KwThrows }
  "case"
    { mkTokConst KwCase }
  "enum"
    { mkTokConst KwEnum }
  "instanceof"
    { mkTokConst KwInstanceof }
  "return"
    { mkTokConst KwReturn }
  "transient"
    { mkTokConst KwTransient }
  "catch"
    { mkTokConst KwCatch }
  "extends"
    { mkTokConst KwExtends }
  "int"
    { mkTokConst KwInt }
  "short"
    { mkTokConst KwShort }
  "try"
    { mkTokConst KwTry }
  "char"
    { mkTokConst KwChar }
  "final"
    { mkTokConst KwFinal }
  "interface"
    { mkTokConst KwInterface }
  "static"
    { mkTokConst KwStatic }
  "void"
    { mkTokConst KwVoid }
  "class"
    { mkTokConst KwClass }
  "finally"
    { mkTokConst KwFinally }
  "long"
    { mkTokConst KwLong }
  "strictfp"
    { mkTokConst KwStrictfp }
  "volatile"
    { mkTokConst KwVolatile }
  "const"
    { mkTokConst KwConst }
  "float"
    { mkTokConst KwFloat }
  "native"
    { mkTokConst KwNative }
  "super"
    { mkTokConst KwSuper }
  "while"
    { mkTokConst KwWhile }
  "_"
    { mkTokConst KwSymbolUnderscore }

  -- Special Identifiers
  "var"
    { mkTokConst IdentVar }
  "yield"
    { mkTokConst IdentYield }
  "record"
    { mkTokConst IdentRecord }

  -- Separators
  "("
    { mkTokConst SepLParen }
  ")"
    { mkTokConst SepRParen }
  "{"
    { mkTokConst SepLBrace }
  "}"
    { mkTokConst SepRBrace }
  "["
    { mkTokConst SepLBracket }
  "]"
    { mkTokConst SepRBracket }
  ";"
    { mkTokConst SepSColon }
  ","
    { mkTokConst SepComma }
  "."
    { mkTokConst SepDot }
  "..."
    { mkTokConst SepTripleDot }
  "@"
    { mkTokConst SepAt }
  "::"
    { mkTokConst SepDbColon }

  -- Operators
  "="
    { mkTokConst OpEq }
  ">"
    { mkTokConst OpGt }
  "<"
    { mkTokConst OpLt }
  "!"
    { mkTokConst OpExclam }
  "~"
    { mkTokConst OpTilde }
  "?"
    { mkTokConst OpQue }
  ":"
    { mkTokConst OpCol }
  "->"
    { mkTokConst OpMinusGt }
  "=="
    { mkTokConst OpEqEq }
  "<="
    { mkTokConst OpLe }
  "!="
    { mkTokConst OpNe }
  "&&"
    { mkTokConst OpAndAnd }
  "||"
    { mkTokConst OpOrOr }
  "++"
    { mkTokConst OpPlusPlus }
  "--"
    { mkTokConst OpMinusMinus }
  "+"
    { mkTokConst OpPlus }
  "-"
    { mkTokConst OpMinus }
  "*"
    { mkTokConst OpStar }
  "/"
    { mkTokConst OpSlash }
  "&"
    { mkTokConst OpAnd }
  "|"
    { mkTokConst OpOr }
  "^"
    { mkTokConst OpCaret }
  "%"
    { mkTokConst OpPercent }
  "<<"
    { mkTokConst OpLtLt }
  "+="
    { mkTokConst OpPlusEq }
  "-="
    { mkTokConst OpMinusEq }
  "*="
    { mkTokConst OpStarEq }
  "/="
    { mkTokConst OpSlashEq }
  "&="
    { mkTokConst OpAndEq }
  "|="
    { mkTokConst OpOrEq }
  "^="
    { mkTokConst OpCaretEq }
  "%="
    { mkTokConst OpPercentEq }
  "<<="
    { mkTokConst OpLtLtEq }

  -- CharacterLiteral
  --   SingleCharacter
  '[^'\\]'
    { mkTok getCharLiteral }
  --   EscapeSequence
  "'"@EscapeBody"'"
    { mkTok getCharLiteral }

  -- StringLiteral
  \"([^\\\"]|@EscapeBody)*\"
    { mkTok getStringLiteral }

  -- TextBlock
  -- See: [Regular expressions] section in /docs/README.md
  \"\"\"($NotDbQuoteNotSlash|\\(\"|\\|$NotDbQuoteNotSlash)|\"($NotDbQuoteNotSlash|\\(\"|\\|$NotDbQuoteNotSlash)|\"($NotDbQuoteNotSlash|\\(\"|\\|$NotDbQuoteNotSlash))))*\"\"\"
    { mkTok getTextBlock }

  -- IntegerLiteral
  --   DecimalIntegerLiteral
  --     note that the regex below is not yet ready to be merged with others
  --     as it would interfere with floating point literals.
  [0-9][_0-9]*@IntegerTypeSuffix?
    { mkTok getIntegerLiteral }
  --   HexIntegerLiteral
  --   OctalIntegerLiteral
  --   BinaryIntegerLiteral
  0[bBxX]?$NumLitPart+@IntegerTypeSuffix?
    { mkTok getIntegerLiteral }

  -- FloatingPointLiteral
  --   DecimalFloatingPointLiteral
  @Digits\.@Digits?@ExponentPart?@FloatTypeSuffix?
    { mkTok getFloatingPoint }
  --   DecimalFloatingPointLiteral
  \.@Digits@ExponentPart?@FloatTypeSuffix?
    { mkTok getFloatingPoint }
  --   DecimalFloatingPointLiteral
  @Digits@ExponentPart@FloatTypeSuffix?
    { mkTok getFloatingPoint }
  --   DecimalFloatingPointLiteral
  @Digits@ExponentPart?@FloatTypeSuffix
    { mkTok getFloatingPoint }
  --  HexadecimalFloatingPointLiteral
  @HexSignificand@BinaryExponent@FloatTypeSuffix?
    { mkTok getFloatingPoint }

  $JavaIdentifierStartLite$JavaIdentifierPartLite*
    { mkTok getIdentifier }
