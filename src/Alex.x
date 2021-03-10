{
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Alex where
}

%wrapper "monad-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]

-- TODO: this is just small example to get project to compile, nothing about Java yet.

tokens :-

  $white+
    ;
  "--".*
    ;
  let
    { \_ _ -> pure () }
  in
    { \_ _ -> pure () }
  $digit+
    { \(_, _, xs, _) l -> pure () }
  [\=\+\-\*\/\(\)]
    { \(_, _, xs, _) 1 -> pure () }
  $alpha [$alpha $digit \_ \']*
    { \(_, _, xs, _) l -> pure () }

{

type Token = ()

alexEOF = pure ()

}
