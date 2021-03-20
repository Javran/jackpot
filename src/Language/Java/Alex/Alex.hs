{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Alex.Alex where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Bits
import Data.Char (ord)
import Data.Word (Word8)
import Language.Java.Alex.Lexer
import Language.Java.Alex.Token

type Byte = Word8

type AlexInput =
  ( AlexPosn -- current position,
  , Char -- previous char
  , [Byte] -- pending bytes on current char
  , String -- current input string
  )

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p, c, _bs, _s) = c

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (p, c, b : bs, s) = Just (b, (p, c, bs, s))
alexGetByte (_, _, [], []) = Nothing
alexGetByte (p, _, [], c : s) =
  let p' = alexMove p c
   in case utf8Encode' c of
        (b, bs) -> p' `seq` Just (b, (p', c, bs, s))

data AlexPosn = AlexPn !Int !Int !Int
  deriving (Eq, Show)

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
  (x, xs) -> (fromIntegral x, map fromIntegral xs)
  where
    go oc
      | oc <= 0x7f =
        ( oc
        , []
        )
      | oc <= 0x7ff =
        ( 0xc0 + (oc `Data.Bits.shiftR` 6)
        , [ 0x80 + oc Data.Bits..&. 0x3f
          ]
        )
      | oc <= 0xffff =
        ( 0xe0 + (oc `Data.Bits.shiftR` 12)
        , [ 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
          , 0x80 + oc Data.Bits..&. 0x3f
          ]
        )
      | otherwise =
        ( 0xf0 + (oc `Data.Bits.shiftR` 18)
        , [ 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
          , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
          , 0x80 + oc Data.Bits..&. 0x3f
          ]
        )

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a + 1) l (c + alex_tab_size - ((c -1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a + 1) (l + 1) 1
alexMove (AlexPn a l c) _ = AlexPn (a + 1) l (c + 1)

alexGetInput :: Alex AlexInput
alexGetInput =
  Alex $ \s@AlexState {alex_pos = pos, alex_chr = c, alex_bytes = bs, alex_inp = inp__} ->
    Right (s, (pos, c, bs, inp__))

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p, c, _ps, s) = (p, c, [], s)

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos, c, bs, inp__) =
  Alex $ \s -> case s {alex_pos = pos, alex_chr = c, alex_bytes = bs, alex_inp = inp__} of
    state__@AlexState {} -> Right (state__, ())

alexMonadScan :: Alex Token
alexMonadScan = do
  inp__ <- alexGetInput

  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError (AlexPn _ line column, _, _, _) ->
      alexError $ "lexical error at line " ++ show line ++ ", column " ++ show column
    AlexSkip inp__' _len -> do
      alexSetInput inp__'
      alexMonadScan
    AlexToken inp__' len action -> do
      alexSetInput inp__'
      action (ignorePendingBytes inp__) len

runAlex :: String -> Alex a -> Either String a
runAlex input__ (Alex f) =
  case f
    (AlexState
       { alex_bytes = []
       , alex_pos = alexStartPos
       , alex_inp = input__
       , alex_chr = '\n'
       , alex_scd = 0
       }) of
    Left msg -> Left msg
    Right (_, a) -> Right a

newtype Alex a = Alex {unAlex :: AlexState -> Either String (AlexState, a)}

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
    Left msg -> Left msg
    Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
    Left msg -> Left msg
    Right (s', f) -> case unAlex a s' of
      Left msg -> Left msg
      Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k = Alex $ \s -> case unAlex m s of
    Left msg -> Left msg
    Right (s', a) -> unAlex (k a) s'

instance MonadError String Alex where
  throwError e = Alex (const (Left e))
  catchError m handler = Alex $ \st -> case unAlex m st of
    Left e -> unAlex (handler e) st
    Right v -> Right v

instance MonadState AlexState Alex where
  state f = Alex $ \s -> let (v, s') = f s in pure (s', v)

data AlexState = AlexState
  { alex_pos :: !AlexPosn -- position at current input location
  , alex_inp :: String -- the current input
  , alex_chr :: !Char -- the character before the input
  , alex_bytes :: [Byte]
  , alex_scd :: !Int -- the current startcode
  }

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState {alex_scd = sc} -> Right (s, sc)

alexEOF :: Alex Token
alexEOF = pure EndOfFile

type AlexAction result = AlexInput -> Int -> Alex result
