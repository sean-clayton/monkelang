module Lexer (Lexer, run, nextToken, tokenFromChar) where

import Text.Printf (printf)
import Token qualified

data Lexer = Lexer
  { input :: String
  , position :: Int
  , ch :: Maybe Char
  }
  deriving (Show, Eq)

{- Create a new lexer from an input -}
run :: String -> Lexer
run input =
  if null input
    then Lexer{input, position = 0, ch = Nothing}
    else Lexer{input, position = 0, ch = Just (head input)}

tokenFromChar ';' = Token.Semicolon
tokenFromChar '(' = Token.LeftParen
tokenFromChar ')' = Token.RightParen
tokenFromChar ',' = Token.Comma
tokenFromChar '+' = Token.Plus
tokenFromChar '-' = Token.Minus
tokenFromChar '/' = Token.Slash
tokenFromChar '*' = Token.Asterisk
tokenFromChar '<' = Token.LessThan
tokenFromChar '>' = Token.GreaterThan
tokenFromChar '{' = Token.LeftBrace
tokenFromChar '}' = Token.RightBrace
tokenFromChar char = error $ printf "unknown char: %c" char

{- Move the lexer to the next token.
   Returns the new lexer after moving, and maybe a token. -}
nextToken lexer =
  case ch lexer of
    Just currentChar -> (advance lexer, Just (tokenFromChar currentChar))
    Nothing -> (lexer, Nothing)

advance lexer =
  if position lexer >= length (input lexer) - 1
    then lexer{ch = Nothing}
    else
      ( let newPosition = position lexer + 1
         in lexer{position = newPosition, ch = Just (input lexer !! newPosition)}
      )