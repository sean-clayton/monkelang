module Token (Token (..), lookupIdent) where

import Prelude hiding (False, True)

data Token
  = Illegal
  | Ident String
  | Integer String
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LessThan
  | GreaterThan
  | Equal
  | NotEqual
  | Comma
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
  deriving (Show, Eq)

lookupIdent "fn" = Function
lookupIdent "let" = Let
lookupIdent "true" = True
lookupIdent "false" = False
lookupIdent "if" = If
lookupIdent "else" = Else
lookupIdent "return" = Return
lookupIdent str = Ident str