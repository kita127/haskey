{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.Text                     as T
import qualified Haskey.Ast                    as Ast
import qualified Haskey.Lexer                  as Lx
import qualified Haskey.Parser                 as Ps
import qualified Haskey.Token                  as Tok
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList
        [ testLetStatement
        , testReturnStatement
        , testIdentifireExpression
        , testIntegerLiteralExpression
        , testParsingPrefixExpressions
        , testParsingInfixExpressions           -- ここまでリファクタ OK
        , testOperatorPrecedenceParsing
        , testBooleanExpression
        , testIfExpression
        , testFunctionLiteral
        , testCallExpressionParsing
        , testLetStatements
        , testReturnStatements
        , testStringLiteral
        , testParsingArrayLiteral
        , testParsingIndexExpressions
        , testInvalid
        ]
    return ()





-- | helper
--
------------------------------------------------------------------------
--
data Exp = ExpIdent T.Text | ExpInt Integer | ExpBool Bool | ExpOp T.Text | ExInvalid
    deriving (Eq, Show)

tProgram :: T.Text -> Ast.Program
tProgram = Ps.parse . Lx.lexicalize

tExpressionStatement :: Ast.Statement -> Either String Ast.Statement
tExpressionStatement e@(Ast.ExpressionStatement _ _) = Right e
tExpressionStatement x                               = Left $ show x

tIndexStatement :: Int -> T.Text -> Ast.Statement
tIndexStatement n = (!! n) . Ast.statements . tProgram

tExpContents :: Ast.Expression -> [Exp]
tExpContents (Ast.Identifire     _ v         ) = [ExpIdent v]
tExpContents (Ast.IntegerLiteral _ v         ) = [ExpInt v]
tExpContents (Ast.Boolean        _ v         ) = [ExpBool v]
tExpContents (Ast.PrefixExpression _ op right) = ExpOp op : tExpContents right
tExpContents (Ast.InfixExpression _ l op right) =
    tExpContents l ++ [ExpOp op] ++ tExpContents right
tExpContents _ = [ExInvalid]




_statements = Ast.statements . Ps.parse . Lx.lexicalize

_firstStatement = head . _statements

fetchFirstExpression =
    Ast.expression . head . Ast.statements . Ps.parse . Lx.lexicalize

isExpStmt (Ast.ExpressionStatement _ _) = Right "ExpressionStatement"
isExpStmt s                             = Left $ Ast.string s



------------------------------------------------------------------------



-- | testLetStatement
--
testLetStatement :: Test
testLetStatement = TestList
    [ "testLetStatement test 1"
    ~:  (name . (!! 0) . Ast.statements . tProgram) input1
    ~?= Right "x"
    , "testLetStatement test 2"
    ~:  (name . (!! 1) . Ast.statements . tProgram) input1
    ~?= Right "y"
    , "testLetStatement test 3"
    ~:  (name . (!! 2) . Ast.statements . tProgram) input1
    ~?= Right "foobar"
    ]
  where
    name (Ast.LetStatement _ n _) = Right $ Ast.expValue n
    name x                        = Left $ show x

    input1 = [r|
    let x = 5;
    let y = 10;
    let foobar = 838383;
    |]


-- | testReturnStatement
--
testReturnStatement :: Test
testReturnStatement = TestList
    [ "testReturnStatement test 1"
    ~:  (contents . (!! 0) . Ast.statements . tProgram) input1
    ~?= Right ("return", 5)
    , "testReturnStatement test 2"
    ~:  (contents . (!! 1) . Ast.statements . tProgram) input1
    ~?= Right ("return", 10)
    , "testReturnStatement test 3"
    ~:  (contents . (!! 2) . Ast.statements . tProgram) input1
    ~?= Right ("return", 993322)
    ]
  where
    contents (Ast.ReturnStatement t v) = Right (Tok.literal t, Ast.intValue v)
    contents x                         = Left $ show x

    input1 = [r|
    return 5;
    return 10;
    return 993322;
    |]



-- | testIdentifireExpression
testIdentifireExpression :: Test
testIdentifireExpression = TestList
    [ "testIdentifireExpression test 1" ~: helper "foobar" ~?= Right "foobar"
    , "testIdentifireExpression test 2" ~: helper "foobar;" ~?= Right "foobar"
    , "testIdentifireExpression test 3" ~: helper "identifire;;;" ~?= Right
        "identifire"
    ]
  where
    tIdentifire (Ast.Identifire _ v) = Right v
    tIdentifire x                    = Left $ show x
    helper a = do
        let x = tIndexStatement 0 a
        y <- tExpressionStatement x
        let z = Ast.expression y
        tIdentifire z


-- | testIntegerLiteralExpression
--
testIntegerLiteralExpression :: Test
testIntegerLiteralExpression = TestList
    ["testIntegerLiteralExpression test 1" ~: helper "5" ~?= Right "5"]
  where
    helper arg = do
        let a = tIndexStatement 0 arg
        b <- tExpressionStatement a
        let c = Ast.expression b
            d = Ast.string c
        return d


-- | testExpressionHelper
--
testExpressionHelper arg = do
    let a = tIndexStatement 0 arg
    b <- tExpressionStatement a
    let c = Ast.expression b
    return $ tExpContents c

-- | testParsingPrefixExpressions
--
testParsingPrefixExpressions :: Test
testParsingPrefixExpressions = TestList
    [ "testParsingPrefixExpressions test 1"
    ~:  testExpressionHelper "!5;"
    ~?= Right [ExpOp "!", ExpInt 5]
    , "testParsingPrefixExpressions test 2"
    ~:  testExpressionHelper "-15;"
    ~?= Right [ExpOp "-", ExpInt 15]
    , "testParsingPrefixExpressions test 3"
    ~:  testExpressionHelper "!true"
    ~?= Right [ExpOp "!", ExpBool True]
    , "testParsingPrefixExpressions test 4"
    ~:  testExpressionHelper "!false"
    ~?= Right [ExpOp "!", ExpBool False]
    ]

-- | testParsingInfixExpressions
--
testParsingInfixExpressions :: Test
testParsingInfixExpressions = TestList
    [ "testParsingInfixExpressions test 1"
    ~:  testExpressionHelper "5 + 5;"
    ~?= Right [ExpInt 5, ExpOp "+", ExpInt 5]
    , "testParsingInfixExpressions test 2"
    ~:  testExpressionHelper "5 - 5;"
    ~?= Right [ExpInt 5, ExpOp "-", ExpInt 5]
    , "testParsingInfixExpressions test 3"
    ~:  testExpressionHelper "5 * 5;"
    ~?= Right [ExpInt 5, ExpOp "*", ExpInt 5]
    , "testParsingInfixExpressions test 4"
    ~:  testExpressionHelper "5 / 5;"
    ~?= Right [ExpInt 5, ExpOp "/", ExpInt 5]
    , "testParsingInfixExpressions test 5"
    ~:  testExpressionHelper "5 > 5;"
    ~?= Right [ExpInt 5, ExpOp ">", ExpInt 5]
    , "testParsingInfixExpressions test 6"
    ~:  testExpressionHelper "5 < 5;"
    ~?= Right [ExpInt 5, ExpOp "<", ExpInt 5]
    , "testParsingInfixExpressions test 7"
    ~:  testExpressionHelper "5 == 5;"
    ~?= Right [ExpInt 5, ExpOp "==", ExpInt 5]
    , "testParsingInfixExpressions test 8"
    ~:  testExpressionHelper "5 != 5;"
    ~?= Right [ExpInt 5, ExpOp "!=", ExpInt 5]
    , "testParsingInfixExpressions test 9"
    ~:  testExpressionHelper "true == true"
    ~?= Right [ExpBool True, ExpOp "==", ExpBool True]
    , "testParsingInfixExpressions test 10"
    ~:  testExpressionHelper "true != false"
    ~?= Right [ExpBool True, ExpOp "!=", ExpBool False]
    , "testParsingInfixExpressions test 11"
    ~:  testExpressionHelper "false == false"
    ~?= Right [ExpBool False, ExpOp "==", ExpBool False]
    ]


-- | testOperatorPrecedenceParsing
--
testOperatorPrecedenceParsing :: Test
testOperatorPrecedenceParsing = TestList
    [ "testOperatorPrecedenceParsing test 1"
    ~:  testHelper "-a * b"
    ~?= "((-a) * b)"
    , testHelper "!-a" ~?= "(!(-a))"
    , testHelper "a + b + c" ~?= "((a + b) + c)"
    , testHelper "a + b - c" ~?= "((a + b) - c)"
    , testHelper "a * b * c" ~?= "((a * b) * c)"
    , testHelper "a * b / c" ~?= "((a * b) / c)"
    , testHelper "a + b / c" ~?= "(a + (b / c))"
    , testHelper "a + b * c + d / e - f" ~?= "(((a + (b * c)) + (d / e)) - f)"
    , testHelper "3 + 4; -5 * 5" ~?= "(3 + 4)((-5) * 5)"
    , testHelper "5 > 4 == 3 < 4" ~?= "((5 > 4) == (3 < 4))"
    , testHelper "5 < 4 != 3 > 4" ~?= "((5 < 4) != (3 > 4))"
    , testHelper "3 + 4 * 5 == 3 * 1 + 4 * 5"
        ~?= "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
    , testHelper "true" ~?= "true"
    , testHelper "false" ~?= "false"
    , testHelper "3 > 5 == false" ~?= "((3 > 5) == false)"
    , testHelper "3 < 5 == true" ~?= "((3 < 5) == true)"
    , "testOperatorPrecedenceParsing paren test 1"
    ~:  testHelper "1 + (2 + 3) + 4"
    ~?= "((1 + (2 + 3)) + 4)"
    , "testOperatorPrecedenceParsing paren test 2"
    ~:  testHelper "(5 + 5) * 2"
    ~?= "((5 + 5) * 2)"
    , "testOperatorPrecedenceParsing paren test 3"
    ~:  testHelper "2 / (5 + 5)"
    ~?= "(2 / (5 + 5))"
    , "testOperatorPrecedenceParsing paren test 4"
    ~:  testHelper "-(5 + 5)"
    ~?= "(-(5 + 5))"
    , "testOperatorPrecedenceParsing paren test 5"
    ~:  testHelper "!(true == true)"
    ~?= "(!(true == true))"
    , "testOperatorPrecedenceParsing call function 1"
    ~:  testHelper "a + add(b * c) + d"
    ~?= "((a + add((b * c))) + d)"
    , "testOperatorPrecedenceParsing call function 2"
    ~:  testHelper "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))"
    ~?= "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"
    , "testOperatorPrecedenceParsing call function 2"
    ~:  testHelper "add(a + b + c * d / f + g)"
    ~?= "add((((a + b) + ((c * d) / f)) + g))"
    , "testOperatorPrecedenceParsing index 1"
    ~:  testHelper "a * [1, 2, 3, 4][b * c] * d"
    ~?= "((a * ([1, 2, 3, 4][(b * c)])) * d)"
    , "testOperatorPrecedenceParsing index 2"
    ~:  testHelper "add(a * b[2], b[1], 2 * [1, 2][1])"
    ~?= "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"
    ]
    where testHelper = Ast.string . Ps.parse . Lx.lexicalize

-- | testBooleanExpression
testBooleanExpression :: Test
testBooleanExpression = TestList
    [ "testBooleanExpression test 1" ~: testHelper "true" ~?= "true"
    , "testBooleanExpression test 2" ~: testHelper "false" ~?= "false"
    ]
    where testHelper = Ast.string . fetchFirstExpression

-- | testIfExpression
--
testIfExpression :: Test
testIfExpression = TestList
    [ "testIfExpression test 1"
    ~:
        --                                      program Statements length
        --                                      condition expression
        --                                      consequence statements length
        --                                      consequence expression
        --                                      is alternative nil
        --                                      (alternative statements length)
        --                                      (alternative expression)
        testHelper1 "if (x < y) { x }"
    ~?= (1, [ExpIdent "x", ExpOp "<", ExpIdent "y"], 1, [ExpIdent "x"], True)
    , "testIfExpression test 2"
    ~:
        --                                      program Statements length
        --                                      condition expression
        --                                      consequence statements length
        --                                      consequence expression
        --                                      is alternative nil
        --                                      (alternative statements length)
        --                                      (alternative expression)
        testHelper2 "if (x < y) { x } else { y }"
    ~?= ( 1
        , [ExpIdent "x", ExpOp "<", ExpIdent "y"]
        , 1
        , [ExpIdent "x"]
        , False
        , 1
        , [ExpIdent "y"]
        )
    ]

  where
    testHelper1 s =
        let prg = (Ps.parse . Lx.lexicalize) s
        in  ( programStatementsLen prg
            , conditionExpression prg
            , consequenceStmtsLen prg
            , consequenceExpContents prg
            , isAlternativeNil prg
            )

    testHelper2 s =
        let prg = (Ps.parse . Lx.lexicalize) s
        in  ( programStatementsLen prg
            , conditionExpression prg
            , consequenceStmtsLen prg
            , consequenceExpContents prg
            , isAlternativeNil prg
            , alternativeStmtsLen prg
            , alternativeExpContents prg
            )
    stmts                = Ast.statements
    stmt                 = head . stmts
    expression           = Ast.expression . stmt
    consequenceStmts     = Ast.stmtStatements . Ast.consequence . expression
    alternativeStmts     = Ast.stmtStatements . Ast.alternative . expression
    programStatementsLen = length . stmts
    conditionExpression  = tExpContents . Ast.condition . expression
    consequenceStmtsLen  = length . consequenceStmts
    consequenceExpContents =
        tExpContents . Ast.expression . head . consequenceStmts
    alternativeStmtsLen = length . alternativeStmts
    alternativeExpContents =
        tExpContents . Ast.expression . head . alternativeStmts
    isAlternativeNil = (== Ast.NilStatement) . Ast.alternative . expression


-- | testFunctionLiteral
--
testFunctionLiteral :: Test
testFunctionLiteral = TestList
    [ "testFunctionLiteral  statements length"
    ~:  (length . _statements) "fn(x, y) { x + y; }"
    ~?= 1
    , "testFunctionLiteral  Is statement expressionStatement"
    ~:  (isExpStmt . _firstStatement) "fn() { x + y; }"
    ~?= Right "ExpressionStatement"
    , "testFunctionLiteral  Is statement expressionStatement"
    ~:  (isExpStmt . _firstStatement) "fn(x, y) { x + y; }"
    ~?= Right "ExpressionStatement"
    , "testFunctionLiteral  Is expression function literal"
    ~:  (isFuncLiteral . fetchFirstExpression) "fn(x, y) { x + y; }"
    ~?= True
    , "testFunctionLiteral  function's param length"
    ~:  (length . parameters) "fn(x, y) { x + y; }"
    ~?= 2
    , "testFunctionLiteral  function's param 1 contents"
    ~:  paramContents 0 "fn(x, y) { x + y; }"
    ~?= [ExpIdent "x"]
    , "testFunctionLiteral  function's param 2 contents"
    ~:  paramContents 1 "fn(x, y) { x + y; }"
    ~?= [ExpIdent "y"]
    , "testFunctionLiteral  function's body statements length"
    ~:  (length . bodyStmts) "fn(x, y) { x + y; }"
    ~?= 1
    , "testFunctionLiteral  Is function's body expression statement"
    ~:  (isExpStmt . head . bodyStmts) "fn(x, y) { x + y; }"
    ~?= Right "ExpressionStatement"
    , "testFunctionLiteral  function's body expression statement contents"
    ~:  (tExpContents . Ast.expression . head . bodyStmts) "fn(x, y) { x + y; }"
    ~?= [ExpIdent "x", ExpOp "+", ExpIdent "y"]
    , "testFunctionParameterParsing 1 parameters length"
    ~:  (length . parameters) "fn() {};"
    ~?= 0
    , "testFunctionParameterParsing 2 parameters length"
    ~:  (length . parameters) "fn(x) {};"
    ~?= 1
    , "testFunctionParameterParsing 2 parameters contents"
    ~:  paramContents 0 "fn(x) {};"
    ~?= [ExpIdent "x"]
    , "testFunctionParameterParsing 3 parameters length"
    ~:  (length . parameters) "fn(x, y, z) {};"
    ~?= 3
    , "testFunctionParameterParsing 3 parameters contents"
    ~:  concatMap (`paramContents` "fn(x, y, z) {};") [0 .. 2]
    ~?= [ExpIdent "x", ExpIdent "y", ExpIdent "z"]
    ]

  where

    isFuncLiteral Ast.FunctionLiteral{} = True
    isFuncLiteral _                     = False

    parameters = Ast.parameters . fetchFirstExpression

    bodyStmts  = Ast.stmtStatements . Ast.body . fetchFirstExpression

    paramContents n = tExpContents . (!! n) . parameters


-- | testCallExpressionParsing
--
testCallExpressionParsing :: Test
testCallExpressionParsing = TestList
    [ "testCallExpressionParsing  statements length"
    ~:  (length . _statements) "add(1, 2 * 3, 4 + 5);"
    ~?= 1
    , "testCallExpressionParsing  Is expression statement"
    ~:  (isExpStmt . head . _statements) "add(1, 2 * 3, 4 + 5);"
    ~?= Right "ExpressionStatement"
    , "testCallExpressionParsing  Is call expression"
    ~:  (isCallExpression . fetchFirstExpression) "add(1, 2 * 3, 4 + 5);"
    ~?= Right True
    , "testCallExpressionParsing  Is function identifire"
    ~:  (tExpContents . Ast.function . fetchFirstExpression)
            "add(1, 2 * 3, 4 + 5);"
    ~?= [ExpIdent "add"]
    , "testCallExpressionParsing  arguments length"
    ~:  (length . Ast.arguments . fetchFirstExpression) "add(1, 2 * 3, 4 + 5);"
    ~?= 3
    , "testCallExpressionParsing  argument 0 contents"
    ~:  argumentsContents 0 "add(1, 2 * 3, 4 + 5);"
    ~?= [ExpInt 1]
    , "testCallExpressionParsing  argument 1 contents"
    ~:  argumentsContents 1 "add(1, 2 * 3, 4 + 5);"
    ~?= [ExpInt 2, ExpOp "*", ExpInt 3]
    , "testCallExpressionParsing  argument 2 contents"
    ~:  argumentsContents 2 "add(1, 2 * 3, 4 + 5);"
    ~?= [ExpInt 4, ExpOp "+", ExpInt 5]
    ]
  where
    isCallExpression Ast.CallExpression{} = Right True
    isCallExpression ex                   = Left $ Ast.string ex

    argumentsContents n =
        tExpContents . (!! n) . Ast.arguments . fetchFirstExpression

-- | testLetStatements
--

testLetStatements :: Test
testLetStatements = TestList
    [ "testLetStatements  statements 1 length"
    ~:  (length . _statements) input1
    ~?= 1
    , "testLetStatements  statements 1 identifire"
    ~:  testId input1
    ~?= [ExpIdent "x"]
    , "testLetStatements  statements 1 value" ~: testValue input1 ~?= [ExpInt 5]
    , "testLetStatements  statements 2 length"
    ~:  (length . _statements) input2
    ~?= 1
    , "testLetStatements  statements 2 identifire"
    ~:  testId input2
    ~?= [ExpIdent "y"]
    , "testLetStatements  statements 2 value"
    ~:  testValue input2
    ~?= [ExpBool True]
    , "testLetStatements  statements 3 length"
    ~:  (length . _statements) input3
    ~?= 1
    , "testLetStatements  statements 3 identifire"
    ~:  testId input3
    ~?= [ExpIdent "foobar"]
    , "testLetStatements  statements 3 value"
    ~:  testValue input3
    ~?= [ExpIdent "y"]
    ]
  where
    input1    = "let x = 5;"
    input2    = "let y = true;"
    input3    = "let foobar = y;"
    testId    = tExpContents . Ast.name . head . _statements
    testValue = tExpContents . Ast.value . head . _statements

-- | testReturnStatements
--
testReturnStatements :: Test
testReturnStatements = TestList
    [ "testReturnStatements  statements 1 length"
    ~:  (length . _statements) input1
    ~?= 1
    , "testReturnStatements  statements 1 return"
    ~:  testReturn input1
    ~?= "return"
    , "testReturnStatements  statements 1 value"
    ~:  testValue input1
    ~?= [ExpInt 5]
    , "testReturnStatements  statements 2 length"
    ~:  (length . _statements) input2
    ~?= 1
    , "testReturnStatements  statements 2 return"
    ~:  testReturn input2
    ~?= "return"
    , "testReturnStatements  statements 2 value"
    ~:  testValue input2
    ~?= [ExpBool True]
    , "testReturnStatements  statements 3 length"
    ~:  (length . _statements) input3
    ~?= 1
    , "testReturnStatements  statements 3 return"
    ~:  testReturn input3
    ~?= "return"
    , "testReturnStatements  statements 3 value"
    ~:  testValue input3
    ~?= [ExpIdent "y"]
    ]
  where
    input1     = "return 5;"
    input2     = "return true;"
    input3     = "return y;"
    testReturn = Tok.literal . Ast.stmtToken . head . _statements
    testValue  = tExpContents . Ast.returnValue . head . _statements


-- | testStringLiteral
--
testStringLiteral :: Test
testStringLiteral = TestList
    [ "testStringLiteral contents"
      ~:  (extractExpression input1 >>= _test)
      ~?= Right "hello world"
    ]
  where
    input1 = [r|"hello world";|]

    _test (Ast.StringLiteral _ s) = Right s
    _test x                       = Left $ Ast.string x

    extractExpression input = case (head . _statements) input of
        (Ast.ExpressionStatement _ expr) -> Right expr
        stms                             -> Left $ Ast.string stms


-- | testParsingArrayLiteral
--
testParsingArrayLiteral :: Test
testParsingArrayLiteral = TestList
    [ "testParsingArrayLiteral 1"
    ~:  _test 0 (fetchFirstExpression input1)
    ~?= Right [ExpInt 1]
    , "testParsingArrayLiteral 2"
    ~:  _test 1 (fetchFirstExpression input1)
    ~?= Right [ExpInt 2, ExpOp "*", ExpInt 2]
    , "testParsingArrayLiteral 3"
    ~:  _test 2 (fetchFirstExpression input1)
    ~?= Right [ExpInt 3, ExpOp "+", ExpInt 3]
    ]
  where
    input1 = "[1, 2 * 2, 3 + 3]"
    _test i (Ast.ArrayLiteral _ els) = Right $ tExpContents $ els !! i
    _test _ x                        = Left $ Ast.string x


-- | testParsingIndexExpressions
--
testParsingIndexExpressions :: Test
testParsingIndexExpressions = TestList
    [ "testParsingIndexExpressions 1 left"
    ~:  _testLeft (fetchFirstExpression input1)
    ~?= Right [ExpIdent "myArray"]
    , "testParsingIndexExpressions 1 index"
    ~:  _testIndex (fetchFirstExpression input1)
    ~?= Right [ExpInt 1, ExpOp "+", ExpInt 1]
    ]
  where
    input1 = "myArray[1 + 1]"
    _testLeft (Ast.IndexExpression _ l _) = Right $ tExpContents l
    _testLeft x                           = Left $ Ast.string x
    _testIndex a@(Ast.IndexExpression _ _ i) = Right $ tExpContents i
    _testIndex x                             = Left $ Ast.string x

-- | testInvalid
--
testInvalid :: Test
testInvalid = TestList
    ["testInvalid  statements 1 length" ~: (length . _statements) "let" ~?= 1]
