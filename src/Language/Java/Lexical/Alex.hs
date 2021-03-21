{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}

module Language.Java.Lexical.Alex where

import Control.Effect.Error
import Control.Effect.State
import qualified Data.Bits
import Data.Char (ord)
import Data.Word (Word8)
import Language.Java.Lexical.Lexer
import Language.Java.Lexical.Token
import Language.Java.PError

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

alexGetInput :: Alex sig m => m AlexInput
alexGetInput =
  gets $
    \AlexState
       { alexPos = pos
       , alexPrevChar = c
       , alexBytes = bs
       , alexInp = inp
       } -> (pos, c, bs, inp)

alexSetInput :: Alex sig m => AlexInput -> m ()
alexSetInput (pos, c, bs, inp) =
  modify $ \s ->
    s
      { alexPos = pos
      , alexPrevChar = c
      , alexBytes = bs
      , alexInp = inp
      }

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p, c, _ps, s) = (p, c, [], s)

alexError :: Alex sig m => String -> m a
alexError msg = throwError $ AlexSimpleError msg

alexMonadScan :: Alex sig m => m Token
alexMonadScan = do
  inp <- alexGetInput
  sc <- gets alexStartCode
  case alexScan inp sc of
    AlexEOF -> pure EndOfFile
    AlexError (AlexPn _ line column, _, _, _) ->
      alexError $ "lexical error at line " ++ show line ++ ", column " ++ show column
    AlexSkip inp' _len -> do
      alexSetInput inp'
      modify $ \s -> s {alexPrevToken = Nothing}
      alexMonadScan
    AlexToken inp' len action -> do
      alexSetInput inp'
      tok <- action (ignorePendingBytes inp) len
      tok <$ modify (\s -> s {alexPrevToken = Just tok})

type Alex sig m =
  ( Has (State AlexState) sig m
  , Has (Error PError) sig m
  )

alexInitState :: String -> AlexState
alexInitState inp =
  AlexState
    { alexBytes = []
    , alexPos = alexStartPos
    , alexInp = inp
    , alexPrevChar = '\n'
    , alexStartCode = 0
    , alexPrevToken = Nothing
    }

data AlexState = AlexState
  { alexPos :: !AlexPosn -- position at current input location
  , alexInp :: String -- the current input
  , alexPrevChar :: !Char -- the character before the input
  , alexBytes :: [Byte]
  , alexStartCode :: !Int -- the current startcode
  , alexPrevToken :: Maybe Token
  }

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1
