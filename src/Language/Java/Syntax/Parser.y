{
module Language.Java.Syntax.Parser where

import Language.Java.Syntax.Ast
import Language.Java.Lexical.Token

}

%name parse
%tokentype { Token }
%error { parseError }

%token
  int { KwInt }

%%

NumericType
  : int { IntType }

{

parseError _ = error "parse error"

}
