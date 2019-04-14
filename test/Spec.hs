{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Text         as T
import qualified Haskey.Ast        as Ast
import qualified Haskey.Lexer      as Lx
import qualified Haskey.Parser     as Ps
import qualified Haskey.Token      as Tk
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList
      [ testLexer
      , testLetStatement
      , testReturnStatement
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
!-/*5;
5 < 10 > 5;
|]

testLexerInput3 = [r|
if (5 < 10) {
    return true;
} else {
    return false;
}|]

testLexerInput4 = [r|
10 == 10;
10 != 9;
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


        , Tk.Token { Tk.tokenType = Tk.Bang , Tk.literal = "!" }
        , Tk.Token { Tk.tokenType = Tk.Minus , Tk.literal = "-" }
        , Tk.Token { Tk.tokenType = Tk.Slash , Tk.literal = "/" }
        , Tk.Token { Tk.tokenType = Tk.Asterisk , Tk.literal = "*" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "5" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }

        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "5" }
        , Tk.Token { Tk.tokenType = Tk.Lt , Tk.literal = "<" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "10" }
        , Tk.Token { Tk.tokenType = Tk.Gt , Tk.literal = ">" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "5" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }

        -- EOF
        , Tk.Token { Tk.tokenType = Tk.Eof , Tk.literal = "" }
        ]

  , "testLexer test 3" ~:
        Lx.lexer testLexerInput3 ~?= [
          Tk.Token { Tk.tokenType = Tk.If , Tk.literal = "if" }
        , Tk.Token { Tk.tokenType = Tk.Lparen , Tk.literal = "(" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "5" }
        , Tk.Token { Tk.tokenType = Tk.Lt , Tk.literal = "<" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "10" }
        , Tk.Token { Tk.tokenType = Tk.Rparen , Tk.literal = ")" }
        , Tk.Token { Tk.tokenType = Tk.Lbrace , Tk.literal = "{" }
        , Tk.Token { Tk.tokenType = Tk.Return , Tk.literal = "return" }
        , Tk.Token { Tk.tokenType = Tk.TRUE , Tk.literal = "true" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }
        , Tk.Token { Tk.tokenType = Tk.Rbrace , Tk.literal = "}" }
        , Tk.Token { Tk.tokenType = Tk.Else , Tk.literal = "else" }
        , Tk.Token { Tk.tokenType = Tk.Lbrace , Tk.literal = "{" }
        , Tk.Token { Tk.tokenType = Tk.Return , Tk.literal = "return" }
        , Tk.Token { Tk.tokenType = Tk.FALSE , Tk.literal = "false" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }
        , Tk.Token { Tk.tokenType = Tk.Rbrace , Tk.literal = "}" }

        -- EOF
        , Tk.Token { Tk.tokenType = Tk.Eof , Tk.literal = "" }
        ]

  , "testLexer test 4" ~:
        Lx.lexer testLexerInput4 ~?= [
          Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "10" }
        , Tk.Token { Tk.tokenType = Tk.Eq , Tk.literal = "==" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "10" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "10" }
        , Tk.Token { Tk.tokenType = Tk.NotEq , Tk.literal = "!=" }
        , Tk.Token { Tk.tokenType = Tk.Int , Tk.literal = "9" }
        , Tk.Token { Tk.tokenType = Tk.Semicolon , Tk.literal = ";" }

        -- EOF
        , Tk.Token { Tk.tokenType = Tk.Eof , Tk.literal = "" }
        ]
  ]

testLetStatementInput1 = [r|
let x = 5;
let y = 10;
let foobar = 838383;
|]

testLetStatement :: Test
testLetStatement = TestList
  [ "testLetStatement test 1" ~:
        (testLetStatementIdentifires ["x", "y", "foobar"] . Ps.parse . Lx.lexer) testLetStatementInput1
            ~?= Right "ok"
  ]

testLetStatementIdentifires :: [T.Text] -> Ast.Program -> Either String String
testLetStatementIdentifires expected ast =
  if actualValues == expected
      then Right "ok"
      else Left (show actualValues)
  where
    actualValues = map (Ast.idValue . Ast.name) (Ast.statements ast)


testReturnStatementInput1 = [r|
return 5;
return 10;
return 993322;
|]

testReturnStatement :: Test
testReturnStatement = TestList
  [ "testReturnStatement test 1" ~:
        (testReturnStatementLiteral . Ps.parse . Lx.lexer) testReturnStatementInput1
            ~?= ["return", "return", "return" ]
  ]

testReturnStatementLiteral :: Ast.Program -> [T.Text]
testReturnStatementLiteral = map (Tk.literal . Ast.stmtToken) . Ast.statements
