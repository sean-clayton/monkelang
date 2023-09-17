import Lexer qualified
import Test.Hspec
import Token qualified

main = hspec $ do
  describe "Lexer.nextToken" $ do
    it "correctly creates empty lexer from an empty string" $ do
      let lexer = Lexer.make ""
       in Lexer.nextToken lexer `shouldBe` (lexer, Nothing)

    it "correctly lexes ';' as Semicolon" $ do
      let lexer = Lexer.make ";"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Semicolon

    it "correctly lexes '(' as LeftParen" $ do
      let lexer = Lexer.make "("
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.LeftParen

    it "correctly lexes ')' as RightParen" $ do
      let lexer = Lexer.make ")"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.RightParen

    it "correctly lexes ',' as Comma" $ do
      let lexer = Lexer.make ","
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Comma

    it "correctly lexes '+' as Plus" $ do
      let lexer = Lexer.make "+"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Plus

    it "correctly lexes '-' as Minus" $ do
      let lexer = Lexer.make "-"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Minus

    it "correctly lexes '/' as Slash" $ do
      let lexer = Lexer.make "/"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Slash

    it "correctly lexes '*' as Asterisk" $ do
      let lexer = Lexer.make "*"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Asterisk

    it "correctly lexes '<' as LessThan" $ do
      let lexer = Lexer.make "<"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.LessThan

    it "correctly lexes '>' as GreaterThan" $ do
      let lexer = Lexer.make ">"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.GreaterThan

    it "correctly lexes '{' as LeftBrace" $ do
      let lexer = Lexer.make "{"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.LeftBrace

    it "correctly lexes '}' as RightBrace" $ do
      let lexer = Lexer.make "}"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.RightBrace

    it "correctly lexes '!' as Bang" $ do
      let lexer = Lexer.make "!"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Bang

    it "correctly lexes '=' as Assign" $ do
      let lexer = Lexer.make "="
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Assign

    it "correctly lexes '!=' as NotEqual" $ do
      let lexer = Lexer.make "!="
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.NotEqual

    it "correctly lexes '==' as Equal" $ do
      let lexer = Lexer.make "=="
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Equal

    it "correctly lexes '1234' as Integer 1234" $ do
      let lexer = Lexer.make "1234"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just (Token.Integer "1234")

    it "correctly lexes 'fn' as Function" $ do
      let lexer = Lexer.make "fn"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Function

    it "correctly lexes 'let' as Let" $ do
      let lexer = Lexer.make "let"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Let

    it "correctly lexes 'true' as True" $ do
      let lexer = Lexer.make "true"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.True

    it "correctly lexes 'false' as False" $ do
      let lexer = Lexer.make "false"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.False

    it "correctly lexes 'if' as If" $ do
      let lexer = Lexer.make "if"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.If

    it "correctly lexes 'else' as Else" $ do
      let lexer = Lexer.make "else"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Else

    it "correctly lexes 'return' as Return" $ do
      let lexer = Lexer.make "return"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just Token.Return

    it "correctly lexes 'asdf' as Ident asdf" $ do
      let lexer = Lexer.make "asdf"
          (_, token) = Lexer.nextToken lexer
       in token `shouldBe` Just (Token.Ident "asdf")