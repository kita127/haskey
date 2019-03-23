{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Haskey.Lexer      as Lx
import qualified Haskey.Token      as Tk
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList
      [ testLexer
      ]
    return ()


testSample :: Test
testSample = TestList
  [ "testSample test 1" ~:
        "hello test" ~?= "hello test"
  ]


testLexerInput2 = [r|let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
|]

testLexer :: Test
testLexer = TestList
  [ "testLexer test 1" ~:
        Lx.lexer "=+(){},;" ~?= [
          Tk.Token { Tk.tokenType = Tk.Assign , Tk.literal = "=" }
        , Tk.Token { Tk.tokenType = Tk.Plus , Tk.literal = "+" }
        , Tk.Token { Tk.tokenType = Tk.Lparen , Tk.literal = "(" }
        , Tk.Token { Tk.tokenType = Tk.Rparen , Tk.literal = ")" }
        , Tk.Token { Tk.tokenType = Tk.Lbrace , Tk.literal = "{" }
        , Tk.Token { Tk.tokenType = Tk.Rbrace , Tk.literal = "}" }
        , Tk.Token { Tk.tokenType = Tk.Comma , Tk.literal = "," }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }
        , Tk.Token { Tk.tokenType = Tk.Eof , Tk.literal = "" }

        ]
  , "testLexer test 2" ~:
        Lx.lexer testLexerInput2 ~?= [
          Tk.Token { Tk.tokenType = Tk.Let , Tk.literal = "let" }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "five" }
        , Tk.Token { Tk.tokenType = Tk.Assign , Tk.literal = "=" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "5" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }

        , Tk.Token { Tk.tokenType = Tk.Let , Tk.literal = "let" }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "ten" }
        , Tk.Token { Tk.tokenType = Tk.Assign , Tk.literal = "=" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "10" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }

        , Tk.Token { Tk.tokenType = Tk.Let , Tk.literal = "let" }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "add" }
        , Tk.Token { Tk.tokenType = Tk.Assign , Tk.literal = "=" }
        , Tk.Token { Tk.tokenType = Tk.Function , Tk.literal = "fn" }
        , Tk.Token { Tk.tokenType = Tk.Lparen , Tk.literal = "(" }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "x" }
        , Tk.Token { Tk.tokenType = Tk.Comma , Tk.literal = "," }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "y" }
        , Tk.Token { Tk.tokenType = Tk.Rparen , Tk.literal = ")" }
        , Tk.Token { Tk.tokenType = Tk.Lbrace , Tk.literal = "{" }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "x" }
        , Tk.Token { Tk.tokenType = Tk.Plus , Tk.literal = "+" }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "y" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }
        , Tk.Token { Tk.tokenType = Tk.Rbrace , Tk.literal = "}" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }

        -- let result = add(five, ten);
        , Tk.Token { Tk.tokenType = Tk.Let , Tk.literal = "let" }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "result" }
        , Tk.Token { Tk.tokenType = Tk.Assign , Tk.literal = "=" }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "add" }
        , Tk.Token { Tk.tokenType = Tk.Lparen , Tk.literal = "(" }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "five" }
        , Tk.Token { Tk.tokenType = Tk.Comma , Tk.literal = "," }
        , Tk.Token { Tk.tokenType = Tk.Ident , Tk.literal = "ten" }
        , Tk.Token { Tk.tokenType = Tk.Rparen , Tk.literal = ")" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }

        -- EOF
        , Tk.Token { Tk.tokenType = Tk.Eof , Tk.literal = "" }
        ]
  ]


