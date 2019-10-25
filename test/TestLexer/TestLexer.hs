{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Haskey.Lexer                  as Lx
import qualified Haskey.Token                  as Tok
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList [testLexer]
    return ()


testLexer :: Test
testLexer = TestList
    [ "testLexer test 1"
    ~:  Lx.lexicalize "=+(){},;"
    ~?= [ Tok.Token { Tok.tokenType = Tok.Assign, Tok.literal = "=" }
        , Tok.Token { Tok.tokenType = Tok.Plus, Tok.literal = "+" }
        , Tok.Token { Tok.tokenType = Tok.Lparen, Tok.literal = "(" }
        , Tok.Token { Tok.tokenType = Tok.Rparen, Tok.literal = ")" }
        , Tok.Token { Tok.tokenType = Tok.Lbrace, Tok.literal = "{" }
        , Tok.Token { Tok.tokenType = Tok.Rbrace, Tok.literal = "}" }
        , Tok.Token { Tok.tokenType = Tok.Comma, Tok.literal = "," }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }
        , Tok.Token { Tok.tokenType = Tok.Eof, Tok.literal = "" }
        ]
    , "testLexer test 2"
    ~:  Lx.lexicalize testLexerInput2
    ~?= [ Tok.Token { Tok.tokenType = Tok.Let, Tok.literal = "let" }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "five" }
        , Tok.Token { Tok.tokenType = Tok.Assign, Tok.literal = "=" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "5" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }
        , Tok.Token { Tok.tokenType = Tok.Let, Tok.literal = "let" }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "ten" }
        , Tok.Token { Tok.tokenType = Tok.Assign, Tok.literal = "=" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "10" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }
        , Tok.Token { Tok.tokenType = Tok.Let, Tok.literal = "let" }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "add" }
        , Tok.Token { Tok.tokenType = Tok.Assign, Tok.literal = "=" }
        , Tok.Token { Tok.tokenType = Tok.Function, Tok.literal = "fn" }
        , Tok.Token { Tok.tokenType = Tok.Lparen, Tok.literal = "(" }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "x" }
        , Tok.Token { Tok.tokenType = Tok.Comma, Tok.literal = "," }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "y" }
        , Tok.Token { Tok.tokenType = Tok.Rparen, Tok.literal = ")" }
        , Tok.Token { Tok.tokenType = Tok.Lbrace, Tok.literal = "{" }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "x" }
        , Tok.Token { Tok.tokenType = Tok.Plus, Tok.literal = "+" }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "y" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }
        , Tok.Token { Tok.tokenType = Tok.Rbrace, Tok.literal = "}" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }

        -- let result = add(five, ten);
        , Tok.Token { Tok.tokenType = Tok.Let, Tok.literal = "let" }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "result" }
        , Tok.Token { Tok.tokenType = Tok.Assign, Tok.literal = "=" }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "add" }
        , Tok.Token { Tok.tokenType = Tok.Lparen, Tok.literal = "(" }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "five" }
        , Tok.Token { Tok.tokenType = Tok.Comma, Tok.literal = "," }
        , Tok.Token { Tok.tokenType = Tok.Ident, Tok.literal = "ten" }
        , Tok.Token { Tok.tokenType = Tok.Rparen, Tok.literal = ")" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }
        , Tok.Token { Tok.tokenType = Tok.Bang, Tok.literal = "!" }
        , Tok.Token { Tok.tokenType = Tok.Minus, Tok.literal = "-" }
        , Tok.Token { Tok.tokenType = Tok.Slash, Tok.literal = "/" }
        , Tok.Token { Tok.tokenType = Tok.Asterisk, Tok.literal = "*" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "5" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "5" }
        , Tok.Token { Tok.tokenType = Tok.Lt, Tok.literal = "<" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "10" }
        , Tok.Token { Tok.tokenType = Tok.Gt, Tok.literal = ">" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "5" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }

        -- EOF
        , Tok.Token { Tok.tokenType = Tok.Eof, Tok.literal = "" }
        ]
    , "testLexer test 3"
    ~:  Lx.lexicalize testLexerInput3
    ~?= [ Tok.Token { Tok.tokenType = Tok.If, Tok.literal = "if" }
        , Tok.Token { Tok.tokenType = Tok.Lparen, Tok.literal = "(" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "5" }
        , Tok.Token { Tok.tokenType = Tok.Lt, Tok.literal = "<" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "10" }
        , Tok.Token { Tok.tokenType = Tok.Rparen, Tok.literal = ")" }
        , Tok.Token { Tok.tokenType = Tok.Lbrace, Tok.literal = "{" }
        , Tok.Token { Tok.tokenType = Tok.Return, Tok.literal = "return" }
        , Tok.Token { Tok.tokenType = Tok.TRUE, Tok.literal = "true" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }
        , Tok.Token { Tok.tokenType = Tok.Rbrace, Tok.literal = "}" }
        , Tok.Token { Tok.tokenType = Tok.Else, Tok.literal = "else" }
        , Tok.Token { Tok.tokenType = Tok.Lbrace, Tok.literal = "{" }
        , Tok.Token { Tok.tokenType = Tok.Return, Tok.literal = "return" }
        , Tok.Token { Tok.tokenType = Tok.FALSE, Tok.literal = "false" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }
        , Tok.Token { Tok.tokenType = Tok.Rbrace, Tok.literal = "}" }

        -- EOF
        , Tok.Token { Tok.tokenType = Tok.Eof, Tok.literal = "" }
        ]
    , "testLexer test 4"
    ~:  Lx.lexicalize testLexerInput4
    ~?= [ Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "10" }
        , Tok.Token { Tok.tokenType = Tok.Eq, Tok.literal = "==" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "10" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "10" }
        , Tok.Token { Tok.tokenType = Tok.NotEq, Tok.literal = "!=" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "9" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }

        -- EOF
        , Tok.Token { Tok.tokenType = Tok.Eof, Tok.literal = "" }
        ]
    , "testLexer test 5 string literal"
    ~:  Lx.lexicalize testLexerInput5
    ~?= [ Tok.Token { Tok.tokenType = Tok.STRING, Tok.literal = "foobar" }
        , Tok.Token { Tok.tokenType = Tok.STRING, Tok.literal = "foo bar" }

        -- EOF
        , Tok.Token { Tok.tokenType = Tok.Eof, Tok.literal = "" }
        ]
    , "testLexer test 6 array"
    ~:  Lx.lexicalize testLexerInput6
    ~?= [ Tok.Token { Tok.tokenType = Tok.Lbracket, Tok.literal = "[" }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "1" }
        , Tok.Token { Tok.tokenType = Tok.Comma, Tok.literal = "," }
        , Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = "2" }
        , Tok.Token { Tok.tokenType = Tok.Rbracket, Tok.literal = "]" }
        , Tok.Token { Tok.tokenType = Tok.Semicolon, Tok.literal = ";" }

        -- EOF
        , Tok.Token { Tok.tokenType = Tok.Eof, Tok.literal = "" }
        ]
    ]
  where
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

    testLexerInput5 = [r|
"foobar"
"foo bar"
|]
    testLexerInput6 = [r|
[1, 2];
|]
