module Language.Java.Alex.UnicodeEscape where

{-
  TODO: impl.

  This technically has nothing to do with Alex,
  but Java spec demands (3.2. Lexical Translations) that Unicode escape to be performed as
  the first step of lexical analysis, which is easier to do outside of Alex's
  framework.

  Therefore UnicodeEscape is here as a pre-processing step prior to the Alex scan.

  According to my understading of the spec spec,
  for \u0000 ~ \uFFFF, we just need to map them to corresponding
  chars, and we don't need to deal with surrogate pairs, which simplifies
  preprocessing.

 -}
