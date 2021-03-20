{-# LANGUAGE DerivingVia #-}

module Language.Java.Alex.Alex where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Bits
import Data.Char (ord)
import Data.Functor.Identity
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

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
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

data AlexPosn = AlexPn !Int !Int !Int
  deriving (Eq, Show)

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a + 1) l (c + alex_tab_size - ((c -1) `rem` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a + 1) (l + 1) 1
alexMove (AlexPn a l c) _ = AlexPn (a + 1) l (c + 1)

alexGetInput :: Alex AlexInput
alexGetInput =
  gets $
    \AlexState
       { alex_pos = pos
       , alex_chr = c
       , alex_bytes = bs
       , alex_inp = inp
       } -> (pos, c, bs, inp)

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos, c, bs, inp) =
  modify $ \s -> s {alex_pos = pos, alex_chr = c, alex_bytes = bs, alex_inp = inp}

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p, c, _ps, s) = (p, c, [], s)

alexError :: String -> Alex a
alexError msg = throwError $ AlexSimpleError msg

alexMonadScan :: Alex Token
alexMonadScan = do
  inp <- alexGetInput
  sc <- gets alex_scd
  case alexScan inp sc of
    AlexEOF -> pure EndOfFile
    AlexError (AlexPn _ line column, _, _, _) ->
      alexError $ "lexical error at line " ++ show line ++ ", column " ++ show column
    AlexSkip inp' _len -> do
      alexSetInput inp'
      alexMonadScan
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) len

runAlex :: String -> Alex a -> Either AlexError a
runAlex inp (Alex f) = fmap fst (f s0)
  where
    s0 =
      AlexState
        { alex_bytes = []
        , alex_pos = alexStartPos
        , alex_inp = inp
        , alex_chr = '\n'
        , alex_scd = 0
        }

data AlexError
  = AlexSimpleError String
  deriving (Show, Eq)

newtype Alex a = Alex
  { unAlex :: AlexState -> Either AlexError (a, AlexState)
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError AlexError
    , MonadState AlexState
    )
    via (StateT AlexState (Except AlexError))

data AlexState = AlexState
  { alex_pos :: !AlexPosn -- position at current input location
  , alex_inp :: String -- the current input
  , alex_chr :: !Char -- the character before the input
  , alex_bytes :: [Byte]
  , alex_scd :: !Int -- the current startcode
  }

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

type AlexAction result = AlexInput -> Int -> Alex result
