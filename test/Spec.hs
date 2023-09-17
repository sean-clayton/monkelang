import Lexer qualified
import Test.Hspec
import Token qualified

main = hspec $ do
  describe "Lexer.tokenFromChar" $ do
    it "correctly lexes a ';'" $ do
      Lexer.tokenFromChar ';' `shouldBe` Token.Semicolon
    it "correctly lexes a '+'" $ do
      Lexer.tokenFromChar '+' `shouldBe` Token.Plus
    it "correctly lexes a '-'" $ do
      Lexer.tokenFromChar '-' `shouldBe` Token.Minus
    it "correctly lexes a '('" $ do
      Lexer.tokenFromChar '(' `shouldBe` Token.LeftParen
    it "correctly lexes a ')'" $ do
      Lexer.tokenFromChar ')' `shouldBe` Token.RightParen
    it "correctly lexes a '{'" $ do
      Lexer.tokenFromChar '{' `shouldBe` Token.LeftBrace
    it "correctly lexes a '}'" $ do
      Lexer.tokenFromChar '}' `shouldBe` Token.RightBrace