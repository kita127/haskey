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
      , testFunctionLiteral
      , testCallExpressionParsing
      , testLetStatements
      , testReturnStatements

      , testInvalid
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

-- | helper
--
_statements = Ast.statements . Ps.parse . Lx.lexer

_firstStatement = head . _statements

fetchFirstExpression = Ast.expression . head . Ast.statements . Ps.parse . Lx.lexer

isExpStmt (Ast.ExpressionStatement _ _ ) = Right "ExpressionStatement"
isExpStmt s                              = Left $ Ast.string s


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

  , "testOperatorPrecedenceParsing call function 1" ~:
        testHelper "a + add(b * c) + d" ~?= "((a + add((b * c))) + d)"

  , "testOperatorPrecedenceParsing call function 2" ~:
        testHelper "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))" ~?= "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"

  , "testOperatorPrecedenceParsing call function 2" ~:
        testHelper "add(a + b + c * d / f + g)" ~?= "add((((a + b) + ((c * d) / f)) + g))"
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


-- | testFunctionLiteral
--
testFunctionLiteral :: Test
testFunctionLiteral = TestList
  [ "testFunctionLiteral  statements length" ~:
         (length . _statements) "fn(x, y) { x + y; }" ~?= 1

  , "testFunctionLiteral  Is statement expressionStatement" ~:
         (isExpStmt . _firstStatement) "fn() { x + y; }" ~?= Right "ExpressionStatement"

  , "testFunctionLiteral  Is statement expressionStatement" ~:
         (isExpStmt . _firstStatement) "fn(x, y) { x + y; }" ~?= Right "ExpressionStatement"

  , "testFunctionLiteral  Is expression function literal" ~:
         (isFuncLiteral . fetchFirstExpression) "fn(x, y) { x + y; }" ~?= True

  , "testFunctionLiteral  function's param length" ~:
         (length . parameters) "fn(x, y) { x + y; }" ~?= 2

  , "testFunctionLiteral  function's param 1 contents" ~:
         paramContents 0 "fn(x, y) { x + y; }" ~?= [ExpIdent "x"]

  , "testFunctionLiteral  function's param 2 contents" ~:
         paramContents 1 "fn(x, y) { x + y; }" ~?= [ExpIdent "y"]

  , "testFunctionLiteral  function's body statements length" ~:
         (length . bodyStmts) "fn(x, y) { x + y; }" ~?= 1

  , "testFunctionLiteral  Is function's body expression statement" ~:
         (isExpStmt . head . bodyStmts) "fn(x, y) { x + y; }" ~?= Right "ExpressionStatement"

  , "testFunctionLiteral  function's body expression statement contents" ~:
         (testExpContents . Ast.expression . head . bodyStmts) "fn(x, y) { x + y; }" ~?=
            [ ExpIdent "x"
            , ExpOp "+"
            , ExpIdent "y"
            ]

  , "testFunctionParameterParsing 1 parameters length" ~:
        (length . parameters) "fn() {};" ~?= 0

  , "testFunctionParameterParsing 2 parameters length" ~:
        (length . parameters) "fn(x) {};" ~?= 1

  , "testFunctionParameterParsing 2 parameters contents" ~:
        paramContents 0 "fn(x) {};" ~?= [ExpIdent "x"]

  , "testFunctionParameterParsing 3 parameters length" ~:
        (length . parameters) "fn(x, y, z) {};" ~?= 3

  , "testFunctionParameterParsing 3 parameters contents" ~:
        concatMap (flip paramContents "fn(x, y, z) {};") [0..2] ~?=
            [ExpIdent "x", ExpIdent "y", ExpIdent "z"]
  ]

  where

    isFuncLiteral (Ast.FunctionLiteral _ _ _) = True
    isFuncLiteral _                           = False

    parameters = Ast.parameters . fetchFirstExpression

    bodyStmts = Ast.stmtStatements . Ast.body . fetchFirstExpression

    paramContents n = (testExpContents . (!! n) . parameters)


-- | testCallExpressionParsing
--
testCallExpressionParsing :: Test
testCallExpressionParsing = TestList
  [ "testCallExpressionParsing  statements length" ~:
         (length . _statements) "add(1, 2 * 3, 4 + 5);" ~?= 1

  , "testCallExpressionParsing  Is expression statement" ~:
         (isExpStmt . head . _statements) "add(1, 2 * 3, 4 + 5);" ~?= Right "ExpressionStatement"

  , "testCallExpressionParsing  Is call expression" ~:
         (isCallExpression . fetchFirstExpression) "add(1, 2 * 3, 4 + 5);" ~?= Right True

  , "testCallExpressionParsing  Is function identifire" ~:
         ( testExpContents . Ast.function . fetchFirstExpression) "add(1, 2 * 3, 4 + 5);" ~?= [ExpIdent "add"]

  , "testCallExpressionParsing  arguments length" ~:
         ( length . Ast.arguments . fetchFirstExpression) "add(1, 2 * 3, 4 + 5);" ~?= 3

  , "testCallExpressionParsing  argument 0 contents" ~:
         argumentsContents 0  "add(1, 2 * 3, 4 + 5);" ~?= [ExpInt 1]

  , "testCallExpressionParsing  argument 1 contents" ~:
         argumentsContents 1  "add(1, 2 * 3, 4 + 5);" ~?= [ExpInt 2, ExpOp "*", ExpInt 3]

  , "testCallExpressionParsing  argument 2 contents" ~:
         argumentsContents 2  "add(1, 2 * 3, 4 + 5);" ~?= [ExpInt 4, ExpOp "+", ExpInt 5]

  ]
  where
    isCallExpression (Ast.CallExpression _ _ _) = Right True
    isCallExpression ex                         = Left $ Ast.string ex

    argumentsContents n = testExpContents . (!! n) . Ast.arguments . fetchFirstExpression

-- | testLetStatements
--

testLetStatements :: Test
testLetStatements = TestList
  [ "testLetStatements  statements 1 length" ~: (length . _statements) input1 ~?= 1

  , "testLetStatements  statements 1 identifire" ~: testId input1 ~?= [ExpIdent "x"]

  , "testLetStatements  statements 1 value" ~: testValue input1 ~?= [ExpInt 5]

  , "testLetStatements  statements 2 length" ~: (length . _statements) input2 ~?= 1

  , "testLetStatements  statements 2 identifire" ~: testId input2 ~?= [ExpIdent "y"]

  , "testLetStatements  statements 2 value" ~: testValue input2 ~?= [ExpBool True]

  , "testLetStatements  statements 3 length" ~: (length . _statements) input3 ~?= 1

  , "testLetStatements  statements 3 identifire" ~: testId input3 ~?= [ExpIdent "foobar"]

  , "testLetStatements  statements 3 value" ~: testValue input3 ~?= [ExpIdent "y"]

  ]
  where
    input1 = "let x = 5;"
    input2 = "let y = true;"
    input3 = "let foobar = y;"
    testId =  testExpContents . Ast.name . head . _statements
    testValue =  testExpContents . Ast.value . head . _statements

-- | testReturnStatements
--
testReturnStatements :: Test
testReturnStatements = TestList
  [ "testReturnStatements  statements 1 length" ~: (length . _statements) input1 ~?= 1

  , "testReturnStatements  statements 1 return" ~: testReturn input1 ~?= "return"

  , "testReturnStatements  statements 1 value" ~: testValue input1 ~?= [ExpInt 5]

  , "testReturnStatements  statements 2 length" ~: (length . _statements) input2 ~?= 1

  , "testReturnStatements  statements 2 return" ~: testReturn input2 ~?= "return"

  , "testReturnStatements  statements 2 value" ~: testValue input2 ~?= [ExpBool True]

  , "testReturnStatements  statements 3 length" ~: (length . _statements) input3 ~?= 1

  , "testReturnStatements  statements 3 return" ~: testReturn input3 ~?= "return"

  , "testReturnStatements  statements 3 value" ~: testValue input3 ~?= [ExpIdent "y"]

  ]
  where
    input1 = "return 5;"
    input2 = "return true;"
    input3 = "return y;"
    testReturn = Tk.literal . Ast.stmtToken . head . _statements
    testValue =  testExpContents . Ast.returnValue . head . _statements


-- | testInvalid
--
testInvalid :: Test
testInvalid = TestList
  [ "testInvalid  statements 1 length" ~: (length . _statements) "let" ~?= 1

  ]
