{-# LANGUAGE FlexibleContexts #-}

module Language.Java.Alex.Token where

import Control.Applicative
import Control.Monad.Except
import Data.Char
import qualified Data.Map.Strict as M
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
  | CharacterLiteral Char
  | StringLiteral String
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
  | Identifier String
  deriving (Eq, Show)

todo :: Applicative f => (a, b, c, String) -> Int -> f Token
todo (_, _, _, xs) l = pure $ Todo (take l xs)

mkTokConst :: Applicative m => Token -> a -> b -> m Token
mkTokConst v _ _ = pure v

mkTok :: Applicative f => (String -> f Token) -> (a, b, c, [Char]) -> Int -> f Token
mkTok act (_, _, _, xs) l = act (take l xs)

{-
  TODO: To be compliant with Java spec
  we need to rule out out-of-bound integers at this stage,
  however it won't be as straightforward due to the interaction with
  prefixing "-" unary operator.
 -}

{-
  Side note: what is the previous char even before Alex start to consume anything?

  Answer: '\n', according to:
  https://github.com/simonmar/alex/blob/35f07a1c272c6b3aace858c2b1b0c427a1d89e67/data/AlexWrappers.hs#L201
 -}

getFloatingPoint :: MonadError String m => String -> m Token
getFloatingPoint inp = case floatingPointLiteralS inp of
  [((s, d), "")] -> pure $ FloatingPointLiteral s d
  _ -> throwError "parse error"

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

getIntegerLiteral :: MonadError String m => String -> m Token
getIntegerLiteral = parseByReadP (uncurry IntegerLiteral <$> integerLitP)

parseByReadP :: MonadError String m => ReadP Token -> String -> m Token
parseByReadP parser inp =
  case readP_to_S (parser <* eof) inp of
    [(tok, "")] ->
      pure tok
    _ -> throwError "parse error"

keywords :: M.Map String Token
keywords =
  M.fromList
    [ ("abstract", KwAbstract)
    , ("continue", KwContinue)
    , ("for", KwFor)
    , ("new", KwNew)
    , ("switch", KwSwitch)
    , ("assert", KwAssert)
    , ("default", KwDefault)
    , ("if", KwIf)
    , ("package", KwPackage)
    , ("synchronized", KwSynchronized)
    , ("boolean", KwBoolean)
    , ("do", KwDo)
    , ("goto", KwGoto)
    , ("private", KwPrivate)
    , ("this", KwThis)
    , ("break", KwBreak)
    , ("double", KwDouble)
    , ("implements", KwImplements)
    , ("protected", KwProtected)
    , ("throw", KwThrow)
    , ("byte", KwByte)
    , ("else", KwElse)
    , ("import", KwImport)
    , ("public", KwPublic)
    , ("throws", KwThrows)
    , ("case", KwCase)
    , ("enum", KwEnum)
    , ("instanceof", KwInstanceof)
    , ("return", KwReturn)
    , ("transient", KwTransient)
    , ("catch", KwCatch)
    , ("extends", KwExtends)
    , ("int", KwInt)
    , ("short", KwShort)
    , ("try", KwTry)
    , ("char", KwChar)
    , ("final", KwFinal)
    , ("interface", KwInterface)
    , ("static", KwStatic)
    , ("void", KwVoid)
    , ("class", KwClass)
    , ("finally", KwFinally)
    , ("long", KwLong)
    , ("strictfp", KwStrictfp)
    , ("volatile", KwVolatile)
    , ("const", KwConst)
    , ("float", KwFloat)
    , ("native", KwNative)
    , ("super", KwSuper)
    , ("while", KwWhile)
    , ("_", KwSymbolUnderscore)
    ]

parseKeywordOrIdentifier :: String -> Maybe Token
parseKeywordOrIdentifier xs =
  (keywords M.!? xs)
    <|> pure (Identifier xs)

getKeywordOrIdentifier :: MonadError String m => String -> m Token
getKeywordOrIdentifier xs = case parseKeywordOrIdentifier xs of
  Nothing -> throwError "Unrecognized keyword or identifer"
  Just t -> pure t

charLitP :: ReadP Char
charLitP = do
  _ <- char '\''
  (do
     ch <- satisfy (`notElem` "\'\\")
     _ <- char '\''
     let v = ord ch
     guard $ v >= 0 && v <= 0xFFFF
     pure ch)
    <++ (do
           -- EscapeSequence
           _ <- char '\\'
           let x <~ y = x <$ char y
           v <-
             '\b' <~ 'b'
               <++ ' ' <~ 's'
               <++ '\t' <~ 't'
               <++ '\n' <~ 'n'
               <++ '\f' <~ 'f'
               <++ '\r' <~ 'r'
               <++ '"' <~ '"'
               <++ '\'' <~ '\''
               <++ '\\' <~ '\\'
               <++ octalEsc
           _ <- char '\''
           pure v)
  where
    octalEsc = do
      xs <- munch1 isOctDigit
      guard $ length xs <= 3
      [(v, "")] <- pure (readOct xs)
      guard $ v >= 0 && v <= (0xFF :: Int)
      pure (chr v)

getCharLiteral :: MonadError String m => String -> m Token
getCharLiteral = parseByReadP (CharacterLiteral <$> charLitP)
