module Lexer (Lexer, make, nextToken) where

import Data.Char (isDigit, isLetter, isSpace)
import Text.Printf (printf)
import Token qualified

data Lexer = Lexer
  { input :: String,
    position :: Int,
    ch :: Maybe Char
  }
  deriving (Show, Eq)

{- Create a new lexer from an input -}
make :: String -> Lexer
make input =
  if null input
    then Lexer {input, position = 0, ch = Nothing}
    else Lexer {input, position = 0, ch = Just (head input)}

{- Move the lexer to the next token.
   Returns the new lexer after moving, and maybe a token. -}
nextToken dirtyLexer =
  let lexer = skipWhitespace dirtyLexer
   in case ch lexer of
        Nothing -> (lexer, Nothing)
        Just currentChar ->
          let (newLexer, token) = case currentChar of
                ';' -> (advance lexer, Token.Semicolon)
                '(' -> (advance lexer, Token.LeftParen)
                ')' -> (advance lexer, Token.RightParen)
                ',' -> (advance lexer, Token.Comma)
                '+' -> (advance lexer, Token.Plus)
                '-' -> (advance lexer, Token.Minus)
                '/' -> (advance lexer, Token.Slash)
                '*' -> (advance lexer, Token.Asterisk)
                '<' -> (advance lexer, Token.LessThan)
                '>' -> (advance lexer, Token.GreaterThan)
                '{' -> (advance lexer, Token.LeftBrace)
                '}' -> (advance lexer, Token.RightBrace)
                '!' -> ifPeeked '=' Token.NotEqual Token.Bang lexer
                '=' -> ifPeeked '=' Token.Equal Token.Assign lexer
                char | isDigit char -> readNumber lexer
                char | isIdentifier char -> readIdentifier lexer
                char -> error $ printf "unknown char: %c" char
           in (newLexer, Just token)

advance lexer =
  if position lexer >= length (input lexer) - 1
    then lexer {ch = Nothing}
    else
      let newPosition = position lexer + 1
       in lexer {position = newPosition, ch = Just (input lexer !! newPosition)}

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

substr start len s = take len (drop start s)

peekChar lexer =
  if position lexer >= length (input lexer) - 1
    then Nothing
    else Just (input lexer !! (position lexer + 1))

ifPeeked char matchToken defaultToken lexer =
  let (newLexer, token) = case peekChar lexer of
        Just peekedChar | peekedChar == char -> (advance lexer, matchToken)
        _ -> (lexer, defaultToken)
   in (newLexer, token)

readWhile lexer condition =
  let posStart = position lexer
      (newLexer, posEnd) = seek lexer (maybe False condition)
      readLength = posEnd - posStart + 1
   in (newLexer, substr posStart readLength (input lexer))

readIdentifier lexer =
  let (newLexer, ident) = readWhile lexer isIdentifier
   in (newLexer, Token.lookupIdent ident)

readNumber lexer =
  let (newLexer, int) = readWhile lexer isDigit
   in (newLexer, Token.Integer int)

isIdentifier = isLetter