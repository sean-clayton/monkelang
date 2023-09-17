module Lexer (Lexer, run, nextToken, tokenFromChar) where

import Data.Char (isLetter, isNumber, isSpace)
import Text.Printf (printf)
import Token qualified

data Lexer = Lexer
  { input :: String,
    position :: Int,
    ch :: Maybe Char,
    tokens :: [Token.Token]
  }
  deriving (Show, Eq)

{- Create a new lexer from an input -}
run :: String -> Lexer
run input =
  if null input
    then Lexer {input, position = 0, ch = Nothing, tokens = []}
    else Lexer {input, position = 0, ch = Just (head input), tokens = []}

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
    then lexer {ch = Nothing}
    else
      ( let newPosition = position lexer + 1
         in lexer {position = newPosition, ch = Just (input lexer !! newPosition)}
      )

seekLoop lexer condition =
  if condition $ ch lexer
    then seekLoop (advance lexer) condition
    else lexer

seek lexer condition =
  let newLexer = seekLoop lexer condition
   in (newLexer, position newLexer)

skipWhitespace lexer =
  let (newLexer, _) = seek lexer (maybe False isSpace)
   in newLexer

substring start len s = take len (drop start s)

readWhile lexer condition =
  let posStart = position lexer
      (newLexer, posEnd) = seek lexer (maybe False condition)
      readLength = posEnd - posStart
   in (newLexer, substring posStart readLength (input lexer))

readIdentifier lexer =
  let (newLexer, ident) = readWhile lexer isIdentifier
   in (newLexer, Token.lookupIdent ident)

readNumber lexer =
  let (newLexer, int) = readWhile lexer isNumber
   in (newLexer, Token.Integer int)

isIdentifier = isLetter