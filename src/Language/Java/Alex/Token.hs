{-# LANGUAGE FlexibleContexts #-}

module Language.Java.Alex.Token where

import Control.Applicative
import Control.Monad.Except
import Data.Char
import Data.Scientific
import Language.Java.Alex.FloatingPoint (floatingPointLiteralS)
import Language.Java.Alex.PlatformFunction
import {-# SOURCE #-} Language.Java.Alex.Alex
import Numeric
import Text.ParserCombinators.ReadP hiding (many)


{-
  Note that operators does not include anything that begins with ">"
  (with the except of ">" alone). This is so that we can
  glue them together when it comes to parsing AST.
  (as Java spec mandates that lexical analysis should not try to
  obtain the longest option in a "type context", however
  there's no easy way to tell a type context during lexical analysis).

  Those excluded operators are:
  >=, >>, >>>, >>=, >>>=

 -}
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
  | SepLParen
  | SepRParen
  | SepLBrace
  | SepRBrace
  | SepLBracket
  | SepRBracket
  | SepSColon
  | SepComma
  | SepDot
  | SepTripleDot
  | SepAt
  | SepDbColon
  | OpEq
  | OpGt
  | OpLt
  | OpExclam
  | OpTilde
  | OpQue
  | OpCol
  | OpMinusGt
  | OpEqEq
  | OpLe
  | OpNe
  | OpAndAnd
  | OpOrOr
  | OpPlusPlus
  | OpMinusMinus
  | OpPlus
  | OpMinus
  | OpStar
  | OpSlash
  | OpAnd
  | OpOr
  | OpCaret
  | OpPercent
  | OpLtLt
  | OpPlusEq
  | OpMinusEq
  | OpStarEq
  | OpSlashEq
  | OpAndEq
  | OpOrEq
  | OpCaretEq
  | OpPercentEq
  | OpLtLtEq
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

  Update: perhaps what we can do is to have a scan on parsed tokens,
  by doing so we directly have access to previous token to tell us just the info we needed.
 -}

{-
  Side note: what is the previous char even before Alex start to consume anything?

  Answer: '\n', according to:
  https://github.com/simonmar/alex/blob/35f07a1c272c6b3aace858c2b1b0c427a1d89e67/data/AlexWrappers.hs#L201
 -}

getFloatingPoint :: Alex sig m => String -> m Token
getFloatingPoint inp = case floatingPointLiteralS inp of
  [((s, d), "")] -> pure $ FloatingPointLiteral s d
  _ -> alexError "parse error"

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

getIntegerLiteral :: Alex sig m => String -> m Token
getIntegerLiteral = parseByReadP (uncurry IntegerLiteral <$> integerLitP)

parseByReadP :: Alex sig m => ReadP Token -> String -> m Token
parseByReadP parser inp =
  case readP_to_S (parser <* eof) inp of
    [(tok, "")] ->
      pure tok
    _ -> alexError "parse error"

mkEscapeBodyP :: ReadP r -> (Char -> r) -> ReadP r
mkEscapeBodyP fallbackP done = do
  _ <- char '\\'
  let x <~ y = done x <$ char y
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
    <++ fallbackP
  where
    octalEsc = do
      xs <- munch1 isOctDigit
      guard $ length xs <= 3
      [(v, "")] <- pure (readOct xs)
      guard $ v >= 0 && v <= (0xFF :: Int)
      pure (done $ chr v)

escapeBodyP :: ReadP Char
escapeBodyP = mkEscapeBodyP pfail id

charLitP :: ReadP Char
charLitP =
  char '\''
    *> (do
          ch <- satisfy (`notElem` "\'\\")
          _ <- char '\''
          let v = ord ch
          guard $ v >= 0 && v <= 0xFFFF
          pure ch)
    <++ (escapeBodyP <* char '\'')

getCharLiteral :: Alex sig m => String -> m Token
getCharLiteral = parseByReadP (CharacterLiteral <$> charLitP)

stringLitP :: ReadP String
stringLitP =
  char '\"'
    *> many
      ((do
          ch <- satisfy (`notElem` "\"\\")
          let v = ord ch
          guard $ v >= 0 && v <= 0xFFFF
          pure ch)
         <++ escapeBodyP)
      <* char '\"'

getStringLiteral :: Alex sig m => String -> m Token
getStringLiteral = parseByReadP (StringLiteral <$> stringLitP)

-- extracts content part from a TextBlock raw literal.
preprocessTextBlock :: String -> Maybe String
preprocessTextBlock xs0 = do
  ("\"\"\"", xs1) <- pure $ splitAt 3 xs0
  '\n' : xs2 <- pure $ dropWhile (\ch -> isSpace ch && ch /= '\n') xs1
  (xs3, "\"\"\"") <- pure $ splitAtEnd 3 xs2
  pure xs3

-- splitAt but from end. works on non-empty l and positive l.
-- inspired by:
-- https://www.joachim-breitner.de/blog/600-On_taking_the_last_n_elements_of_a_list
splitAtEnd :: Int -> [a] -> ([a], [a])
splitAtEnd n l = go (drop n l) $ zipWith (\i _ -> splitAt i l) [0 ..] l
  where
    go [] r = if null r then (l, []) else head r
    go (_ : xs) (_ : ys) = go xs ys
    go (_ : _) [] =
      {-
        for `go xs ys`, `ys` can never exhaust before `xs`,
        as `xs == drop n ys`.
       -}
      error "unreachable"

textBlockBodyP :: ReadP String
textBlockBodyP =
  concat
    <$> many
      ((do
          ch <- satisfy (/= '\\')
          let v = ord ch
          guard $ v >= 0 && v <= 0xFFFF
          pure [ch])
         <++ escapeBodyP')
  where
    escapeBodyP' :: ReadP String
    escapeBodyP' = mkEscapeBodyP ("" <$ char '\n') (: [])

{-
  Quoting from spec:

  > 1. Line terminators are normalized to the ASCII LF character, as follows:
  >   + An ASCII CR character followed by an ASCII LF character is translated to an ASCII LF character.
  >   + An ASCII CR character is translated to an ASCII LF character.

  This part is already done by normalizing line terminators prior to processing in Alex.

  > 2. Incidental white space is removed, as if by execution of String.stripIndent on the characters resulting from step 1.

  `stripIndent` is done as preprocessing step.

  > 3. Escape sequences are interpreted, as if by execution of String.translateEscapes on the characters resulting from step 2.

  This is done by `ReadP` based parsing on actual content.
 -}
getTextBlock :: Alex sig m => String -> m Token
getTextBlock raw = case preprocessTextBlock raw of
  Just content ->
    parseByReadP (StringLiteral <$> textBlockBodyP) (stripIndent content)
  Nothing ->
    alexError "parse error"
