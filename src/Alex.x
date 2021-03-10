{
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Alex where
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
    { \(_, _, xs, _) l -> pure Placeholder }

{

data Token
  = Placeholder
  | BooleanLiteral Bool
  | IntegerLiteral Integer Bool {- whether IntegerTypeSuffix is present -}
  | NullLiteral
  | EndOfFile

alexEOF = pure EndOfFile

}
