{-# LANGUAGE FlexibleContexts #-}

module Language.Java.Alex.Token where

import Control.Applicative
import Control.Monad.Except
import Data.Char
import Data.Scientific
import Language.Java.Alex.FloatingPoint (floatingPointLiteralS)
import Numeric
import Text.ParserCombinators.ReadP

data Token
  = EndOfFile
  | Todo String
  | BooleanLiteral Bool
  | IntegerLiteral Integer Bool {- whether IntegerTypeSuffix is present (True=Long) -}
  | FloatingPointLiteral Scientific Bool {- whether this is double (True) or float (False) -}
  | NullLiteral
  | KwAbstract
  | KwContinue
  | KwFor
  | KwNew
  | KwSwitch
  | KwAssert
  | KwDefault
  | KwIf
  | KwPackage
  | KwSynchronized
  | KwBoolean
  | KwDo
  | KwGoto
  | KwPrivate
  | KwThis
  | KwBreak
  | KwDouble
  | KwImplements
  | KwProtected
  | KwThrow
  | KwByte
  | KwElse
  | KwImport
  | KwPublic
  | KwThrows
  | KwCase
  | KwEnum
  | KwInstanceof
  | KwReturn
  | KwTransient
  | KwCatch
  | KwExtends
  | KwInt
  | KwShort
  | KwTry
  | KwChar
  | KwFinal
  | KwInterface
  | KwStatic
  | KwVoid
  | KwClass
  | KwFinally
  | KwLong
  | KwStrictfp
  | KwVolatile
  | KwConst
  | KwFloat
  | KwNative
  | KwSuper
  | KwWhile
  | KwSymbolUnderscore
  deriving (Eq, Show)

todo :: Applicative f => (a, b, c, String) -> Int -> f Token
todo (_, _, _, xs) l = pure $ Todo (take l xs)

{-
  TODO: To be compliant with Java spec
  we need to rule out out-of-bound integers at this stage,
  however it won't be as straightforward due to the interaction with
  prefixing "-" unary operator.
 -}

parseByRead :: MonadError String m => ReadS Integer -> Char -> String -> m Token
parseByRead _ prevChar _
  | isDigit prevChar =
    {-
     This is weird case is necessary to handle parsing octal literals correctly:

     Without special handling, things like "08" or "0123489", which is supposed to be
     invalid octals are tokenized as two literals: (0 and then 8) or (01234 and then 89).
     To prevent this from happening, decimal tokenizer should check whether previous char is a digit
     and reject if that is indeed the case.

     TODO: well, actually what happened was not a recover at failure -
     of course `[0-7}+` can only match "01234" part.
     what we should do is to instead accept a wider range of input
     and perform extra verifications in Haskell - as regex doesn't have much
     in terms of error reporting, it's more helpful and expressive to do those validations
     on Haskell's side.

    -}
    throwError "integer literal cannot directly follow any digit"
parseByRead reader _ inp =
  case reader $ filter (/= '_') inp of
    [(v, mayL)]
      | mayL `elem` ["", "L", "l"] ->
        pure $ IntegerLiteral v (mayL /= "")
    _ -> throwError "parse error"

{-
  Side note: what is the previous char even before Alex start to consume anything?

  Answer: '\n', according to:
  https://github.com/simonmar/alex/blob/35f07a1c272c6b3aace858c2b1b0c427a1d89e67/data/AlexWrappers.hs#L201
 -}

getFloatingPoint :: MonadError String m => Char -> String -> Int -> m Token
getFloatingPoint prevChar _ _
  | isDigit prevChar =
    throwError "floating point literal cannot directly follow any digit"
getFloatingPoint _ xs l = case floatingPointLiteralS inp of
  [((s, d), "")] -> pure $ FloatingPointLiteral s d
  _ -> throwError "parse error"
  where
    inp = take l xs

oneOf :: [] Char -> ReadP Char
oneOf xs = satisfy (`elem` xs)

intTypeSuffixP :: ReadP Bool
intTypeSuffixP =
  (True <$ oneOf "Ll")
    <++ pure False

{-
  Parses a non-zero decimal integer literal
 -}
nonZeroDecimalIntegerLitP :: ReadP (Integer, Bool)
nonZeroDecimalIntegerLitP = do
  hd <- satisfy (\ch -> isDigit ch && ch /= '0')
  tl <- munch (\ch -> isDigit ch || ch == '_')
  guard $ null tl || isDigit (last tl)
  [(v, "")] <- pure $ readDec (hd : filter (/= '_') tl)
  suf <- intTypeSuffixP
  pure (v, suf)

{-
  Parses either 0 or an octal literal,
  assuming a prefix '0' has been recognized.
 -}
zeroOrOctalIntegerLitP :: ReadP (Integer, Bool)
zeroOrOctalIntegerLitP = do
  xs <- munch (\ch -> isOctDigit ch || ch == '_')
  v <-
    if null xs
      then pure 0 -- whether this is decimal or octal doesn't matter.
      else do
        guard $ head xs /= '_' && last xs /= '_'
        -- parse octal literal body
        [(t, "")] <- pure (readOct (filter (/= '_') xs))
        pure t
  suf <- intTypeSuffixP
  pure (v, suf)

{-
  Parses a hex literal,
  assuming a prefix "0x" or "0X" has been recognized.
 -}
hexIntegerLitP :: ReadP (Integer, Bool)
hexIntegerLitP = do
  hd <- satisfy isHexDigit
  tl <- munch (\ch -> isHexDigit ch || ch == '_')
  guard $ null tl || isHexDigit (last tl)
  [(v, "")] <- pure $ readHex (hd : filter (/= '_') tl)
  suf <- intTypeSuffixP
  pure (v, suf)

{-
  Parses a binary literal,
  assuming a prefix "0b" or "0B" has been recognized.
 -}
binaryIntegerLitP :: ReadP (Integer, Bool)
binaryIntegerLitP = do
  hd <- oneOf "01"
  tl <- munch (`elem` "01_")
  guard $ null tl || last tl /= '_'
  let readBin = readInt 2 (`elem` "01") (\ch -> ord ch - ord '0')
  [(v, "")] <- pure $ readBin (hd : filter (/= '_') tl)
  suf <- intTypeSuffixP
  pure (v, suf)

integerLitP :: ReadP (Integer, Bool)
integerLitP =
  nonZeroDecimalIntegerLitP
    <|> (do
           _ <- char '0'
           (oneOf "xX" *> hexIntegerLitP)
             <++ (oneOf "bB" *> binaryIntegerLitP)
             <++ zeroOrOctalIntegerLitP)

getIntegerLiteral :: MonadError String m => Char -> String -> m Token
getIntegerLiteral prevCh = parseByRead' (readP_to_S (integerLitP <* eof)) prevCh

-- TODO: reduce duplication
parseByRead' :: MonadError String m => ReadS (Integer, Bool) -> Char -> String -> m Token
parseByRead' _ prevChar _
  | isDigit prevChar =
    {-
     This is weird case is necessary to handle parsing octal literals correctly:

     Without special handling, things like "08" or "0123489", which is supposed to be
     invalid octals are tokenized as two literals: (0 and then 8) or (01234 and then 89).
     To prevent this from happening, decimal tokenizer should check whether previous char is a digit
     and reject if that is indeed the case.

     TODO: well, actually what happened was not a recover at failure -
     of course `[0-7}+` can only match "01234" part.
     what we should do is to instead accept a wider range of input
     and perform extra verifications in Haskell - as regex doesn't have much
     in terms of error reporting, it's more helpful and expressive to do those validations
     on Haskell's side.

    -}
    throwError "integer literal cannot directly follow any digit"
parseByRead' reader _ inp =
  case reader inp of
    [((v, f), "")] ->
      pure $ IntegerLiteral v f
    _ -> throwError "parse error"
