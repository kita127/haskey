{-# LANGUAGE OverloadedStrings #-}
import qualified Haskey.Token as Tk
import qualified Haskey.Lexer as Lx
import           Test.HUnit


main :: IO ()
main = do
    runTestTT $ TestList
      [ testSample
      ]
    return ()


testSample :: Test
testSample = TestList
  [ "testSample test 1" ~:
        "hello test" ~?= "hello test"
  ]


testLexerInput = "=+(){},;"
testLexer :: Test
testLexer = TestList
  [ "testLexer test 1" ~:
        Lx.lexer testLexerInput ~?= [
          Tk.Token {
            Tk.tokenType = Tk.Assign
          , Tk.literal = "="
          }

        , Tk.Token {
            Tk.tokenType = Tk.Plus
          , Tk.literal = "+"
          }

        , Tk.Token {
            Tk.tokenType = Tk.Lparen
          , Tk.literal = "("
          }

        , Tk.Token {
            Tk.tokenType = Tk.Rparen
          , Tk.literal = ")"
          }

        , Tk.Token {
            Tk.tokenType = Tk.Lbrace
          , Tk.literal = "{"
          }

        , Tk.Token {
            Tk.tokenType = Tk.Rbrace
          , Tk.literal = "}"
          }

        , Tk.Token {
            Tk.tokenType = Tk.Comma
          , Tk.literal = ","
          }

        , Tk.Token {
            Tk.tokenType = Tk.Semicolon
          , Tk.literal = ";"
          }

        ]
  ]
