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
      , testIdentifireExpression
      , testIntegerLiteralExpression
      , testParsingPrefixExpressions
      , testParsingInfixExpressions
      , testOperatorPrecedenceParsing
      , testBooleanExpression
      , testIfExpression
      ]
    return ()


testSample :: Test
testSample = TestList
  [ "testSample test 1" ~:
        "hello test" ~?= "hello test"
  ]


-- | lexer
-- ---------------------------------------------------------------------------------

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

-- | parser
-- ---------------------------------------------------------------------------------

testLetStatementInput1 = [r|
let x = 5;
let y = 10;
let foobar = 838383;
|]

testLetStatement :: Test
testLetStatement = TestList
  [ "testLetStatement test 1" ~:
        (map testIdentifireName . Ast.statements . Ps.parse . Lx.lexer) testLetStatementInput1
            ~?= ["x", "y", "foobar"]
  ]
  where
    testIdentifireName :: Ast.Statement -> T.Text
    testIdentifireName (Ast.LetStatement _ n v)  = Ast.expValue n
    testIdentifireName f@(Ast.FailStatement _ _) = Ast.string f


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
  where
    testReturnStatementLiteral :: Ast.Program -> [T.Text]
    testReturnStatementLiteral = map (Tk.literal . Ast.stmtToken) . Ast.statements


data Exp = ExpIdent T.Text | ExpInt Integer | ExpBool Bool | ExpOp T.Text
    deriving (Eq, Show)

-- | testExpContents
--
testExpContents :: Ast.Expression -> [Exp]
testExpContents (Ast.Identifire _ v)             = [ExpIdent v]
testExpContents (Ast.IntegerLiteral _ v)         = [ExpInt v]
testExpContents (Ast.Boolean _ v)                = [ExpBool v]
testExpContents (Ast.PrefixExpression _ op r)    =  [ExpOp op] ++ testExpContents r
testExpContents (Ast.InfixExpression _ l op r)   =  testExpContents l ++ [ExpOp op] ++ testExpContents r

-- | fetchFirstExpression
--
fetchFirstExpression = Ast.expression . head . Ast.statements . Ps.parse . Lx.lexer



-- | testIdentifireExpression
testIdentifireExpression :: Test
testIdentifireExpression = TestList
  [ "testIdentifireExpression test 1" ~: testHelper "foobar" ~?= "foobar"
  , "testIdentifireExpression test 2" ~: testHelper "foobar;" ~?= "foobar"
  ]
  where
    testHelper = Ast.string . fetchFirstExpression

-- | testIntegerLiteralExpression
--
testIntegerLiteralExpression :: Test
testIntegerLiteralExpression = TestList
  [ "testIntegerLiteralExpression test 1" ~: (Ast.string . fetchFirstExpression) "5" ~?= "5" ]



-- | testParsingPrefixExpressions
--
testParsingPrefixExpressions :: Test
testParsingPrefixExpressions = TestList
  [ "testParsingPrefixExpressions test 1" ~:
        testHelper "!5;" ~?= [ExpOp "!", ExpInt 5]

  , "testParsingPrefixExpressions test 2" ~:
        testHelper "-15;" ~?= [ExpOp "-", ExpInt 15]

  , "testParsingPrefixExpressions test 3" ~:
        testHelper "!true" ~?= [ExpOp "!", ExpBool True]

  , "testParsingPrefixExpressions test 4" ~:
        testHelper "!false" ~?= [ ExpOp "!", ExpBool False]
  ]
  where
    testHelper = testExpContents . fetchFirstExpression


-- | testParsingInfixExpressions
--
testParsingInfixExpressions :: Test
testParsingInfixExpressions = TestList
  [ "testParsingInfixExpressions test 1" ~:
        testHelper "5 + 5;" ~?= [ExpInt 5, ExpOp "+", ExpInt 5]

  ,     testHelper "5 - 5;" ~?= [ExpInt 5, ExpOp "-", ExpInt 5]

  ,     testHelper "5 * 5;" ~?= [ExpInt 5, ExpOp "*", ExpInt 5]

  ,     testHelper "5 / 5;" ~?= [ExpInt 5, ExpOp "/", ExpInt 5]

  ,     testHelper "5 > 5;" ~?= [ExpInt 5, ExpOp ">", ExpInt 5]

  ,     testHelper "5 < 5;" ~?= [ExpInt 5, ExpOp "<", ExpInt 5]

  ,     testHelper "5 == 5;" ~?= [ExpInt 5, ExpOp "==", ExpInt 5]

  ,     testHelper "5 != 5;" ~?= [ExpInt 5, ExpOp "!=", ExpInt 5]

  ,     testHelper "true == true" ~?= [ExpBool True, ExpOp "==", ExpBool True]

  ,     testHelper "true != false" ~?= [ExpBool True, ExpOp "!=", ExpBool False]

  ,     testHelper "false == false" ~?= [ExpBool False, ExpOp "==", ExpBool False]

  ]
  where
    testHelper = testExpContents . fetchFirstExpression


-- | testOperatorPrecedenceParsing
--
testOperatorPrecedenceParsing :: Test
testOperatorPrecedenceParsing = TestList
  [ "testOperatorPrecedenceParsing test 1" ~:
        testHelper "-a * b" ~?= "((-a) * b)"
  ,     testHelper "!-a" ~?= "(!(-a))"
  ,     testHelper "a + b + c" ~?= "((a + b) + c)"
  ,     testHelper "a + b - c" ~?= "((a + b) - c)"
  ,     testHelper "a * b * c" ~?= "((a * b) * c)"
  ,     testHelper "a * b / c" ~?= "((a * b) / c)"
  ,     testHelper "a + b / c" ~?= "(a + (b / c))"
  ,     testHelper "a + b * c + d / e - f" ~?= "(((a + (b * c)) + (d / e)) - f)"
  ,     testHelper "3 + 4; -5 * 5" ~?= "(3 + 4)((-5) * 5)"
  ,     testHelper "5 > 4 == 3 < 4" ~?= "((5 > 4) == (3 < 4))"
  ,     testHelper "5 < 4 != 3 > 4" ~?= "((5 < 4) != (3 > 4))"
  ,     testHelper "3 + 4 * 5 == 3 * 1 + 4 * 5" ~?= "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"

  ,     testHelper "true" ~?= "true"
  ,     testHelper "false" ~?= "false"
  ,     testHelper "3 > 5 == false" ~?= "((3 > 5) == false)"
  ,     testHelper "3 < 5 == true" ~?= "((3 < 5) == true)"

  , "testOperatorPrecedenceParsing paren test 1" ~:
        testHelper "1 + (2 + 3) + 4" ~?= "((1 + (2 + 3)) + 4)"
  , "testOperatorPrecedenceParsing paren test 2" ~:
        testHelper "(5 + 5) * 2" ~?= "((5 + 5) * 2)"
  , "testOperatorPrecedenceParsing paren test 3" ~:
        testHelper "2 / (5 + 5)" ~?= "(2 / (5 + 5))"
  , "testOperatorPrecedenceParsing paren test 4" ~:
        testHelper "-(5 + 5)" ~?= "(-(5 + 5))"
  , "testOperatorPrecedenceParsing paren test 5" ~:
        testHelper "!(true == true)" ~?= "(!(true == true))"
  ]
  where
    testHelper = Ast.string . Ps.parse . Lx.lexer

-- | testBooleanExpression
testBooleanExpression :: Test
testBooleanExpression = TestList
  [ "testBooleanExpression test 1" ~:
        testHelper "true" ~?= "true"

  , "testBooleanExpression test 2" ~:
        testHelper "false" ~?= "false"
  ]
  where
    testHelper = Ast.string . fetchFirstExpression

-- | testIfExpression
--
testIfExpression :: Test
testIfExpression = TestList
  [ "testIfExpression test 1" ~:
        --                                      program Statements length
        --                                      condition expression
        --                                      consequence statements length
        --                                      consequence expression
        --                                      is alternative nil
        --                                      (alternative statements length)
        --                                      (alternative expression)
        testHelper1 "if (x < y) { x }" ~?= ( 1
                                          , [ExpIdent "x", ExpOp "<", ExpIdent "y"]
                                          , 1
                                          , [ExpIdent "x"]
                                          , True
                                          )
  , "testIfExpression test 2" ~:
        --                                      program Statements length
        --                                      condition expression
        --                                      consequence statements length
        --                                      consequence expression
        --                                      is alternative nil
        --                                      (alternative statements length)
        --                                      (alternative expression)
        testHelper2 "if (x < y) { x } else { y }" ~?= ( 1
                                           , [ExpIdent "x", ExpOp "<", ExpIdent "y"]
                                           , 1
                                           , [ExpIdent "x"]
                                           , False
                                           , 1
                                           , [ExpIdent "y"]
                                           )
  ]

  where
    testHelper1 s = let prg = (Ps.parse . Lx.lexer) s in ( programStatementsLen prg
                                                         , conditionExpression prg
                                                         , consequenceStmtsLen prg
                                                         , consequenceExpContents prg
                                                         , isAlternativeNil prg
                                                         )

    testHelper2 s = let prg = (Ps.parse . Lx.lexer) s in ( programStatementsLen prg
                                                         , conditionExpression prg
                                                         , consequenceStmtsLen prg
                                                         , consequenceExpContents prg
                                                         , isAlternativeNil prg
                                                         , alternativeStmtsLen prg
                                                         , alternativeExpContents prg
                                                         )
    stmts = Ast.statements
    stmt = head . stmts
    expression = Ast.expression . stmt
    consequenceStmts = Ast.stmtStatements . Ast.consequence . expression
    alternativeStmts = Ast.stmtStatements . Ast.alternative . expression
    programStatementsLen = length . stmts
    conditionExpression = testExpContents . Ast.condition . expression
    consequenceStmtsLen = length . consequenceStmts
    consequenceExpContents = testExpContents . Ast.expression . head . consequenceStmts
    alternativeStmtsLen = length . alternativeStmts
    alternativeExpContents = testExpContents . Ast.expression . head . alternativeStmts
    isAlternativeNil =  (== Ast.NilStatement) . Ast.alternative . expression

