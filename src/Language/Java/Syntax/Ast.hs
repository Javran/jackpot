module Language.Java.Syntax.Ast where

data JavaType
  = PrimitiveType PrimitiveType
  | ReferenceType

data PrimitiveType
  = NumericType NumericType
  | BooleanType

data NumericType
  = ByteType
  | ShortType
  | IntType
  | LongType
  | CharType
  | FloatType
  | DoubleType
