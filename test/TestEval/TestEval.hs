{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Map          as M
import qualified Data.Text         as T
import qualified Haskey.Ast        as Ast
import qualified Haskey.Evaluator  as Evl
import qualified Haskey.Lexer      as Lex
import qualified Haskey.Object     as Obj
import qualified Haskey.Parser     as Prs
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList
        [ testEvalIntegerExpression
        , testEvalBooleanExpression
        , testBangOperator
        , testIfElseExpressions
        , testReturnStatements
        , testErrorHandling
        , testLetStatement
        , testFunctionObject
        , testFunctionApplication
        , testStringLiteral
        , testStringConcatenation
        , testBuiltinFunctions
        , testArrayLiterals
        , testArrayIndexExpressions
        , testBuiltinDrive
        ]
    return ()


data Ob = ObInt Integer | ObStr T.Text | ObNull | ObArray [Ob] | ObErr String| Unexpected String
    deriving (Eq, Show)

-- | support
--
_program :: T.Text -> Ast.Program
_program = Prs.parse . Lex.lexicalize

_evaluator = Evl.eval . _program

_object :: T.Text -> Obj.Object
_object s = case Evl.runEvaluator (_evaluator s) Obj.newEnvironment of
    (Evl.Done obj _)  -> obj
    (Evl.Error err _) -> err

_environment :: T.Text -> Either Obj.Environment Obj.Environment
_environment s = case Evl.runEvaluator (_evaluator s) Obj.newEnvironment of
    (Evl.Done _ e)  -> Right e
    (Evl.Error _ e) -> Left e

_boolValue :: T.Text -> Bool
_boolValue = Obj.boolVal . _object

_intValue :: T.Text -> Integer
_intValue = Obj.intVal . _object


_evalObject :: T.Text -> Ob
_evalObject = convOb . _object

convOb :: Obj.Object -> Ob
convOb       Obj.Null          = ObNull
convOb       (Obj.Integer v)   = ObInt v
convOb       (Obj.String v)    = ObStr v
convOb       (Obj.Array es)    = ObArray (map convOb es)
convOb       (Obj.Error   msg) = ObErr msg
convOb       o                 = Unexpected $ show $ Obj.getObjectType o

-- | testEvalIntegerExpression
--
testEvalIntegerExpression :: Test
testEvalIntegerExpression = TestList
    [ "testEvalIntegerExpression 1 / Is object integer?"
    ~:  isIntegerObj "5"
    ~?= Right True
    , "testEvalIntegerExpression 1 / integer value" ~: _intValue "5" ~?= 5
    , "testEvalIntegerExpression 2 / Is object integer?"
    ~:  isIntegerObj "10"
    ~?= Right True
    , "testEvalIntegerExpression 2 / integer value" ~: _intValue "10" ~?= 10
    , "testEvalIntegerExpression 3 / Is object integer?"
    ~:  isIntegerObj "-5"
    ~?= Right True
    , "testEvalIntegerExpression 3 / integer value" ~: _intValue "-5" ~?= -5
    , "testEvalIntegerExpression 4 / Is object integer?"
    ~:  isIntegerObj "-10"
    ~?= Right True
    , "testEvalIntegerExpression 4 / integer value" ~: _intValue "-10" ~?= -10
    , "testEvalIntegerExpression 5 / Is object integer?"
    ~:  isIntegerObj input5
    ~?= Right True
    , "testEvalIntegerExpression 5 / integer value" ~: _intValue input5 ~?= 10
    , "testEvalIntegerExpression 6 / Is object integer?"
    ~:  isIntegerObj input6
    ~?= Right True
    , "testEvalIntegerExpression 6 / integer value" ~: _intValue input6 ~?= 32
    , "testEvalIntegerExpression 7 / Is object integer?"
    ~:  isIntegerObj input7
    ~?= Right True
    , "testEvalIntegerExpression 7 / integer value" ~: _intValue input7 ~?= 0
    , "testEvalIntegerExpression 8 / Is object integer?"
    ~:  isIntegerObj input8
    ~?= Right True
    , "testEvalIntegerExpression 8 / integer value" ~: _intValue input8 ~?= 20
    , "testEvalIntegerExpression 9 / Is object integer?"
    ~:  isIntegerObj input9
    ~?= Right True
    , "testEvalIntegerExpression 9 / integer value" ~: _intValue input9 ~?= 25
    , "testEvalIntegerExpression 10 / Is object integer?"
    ~:  isIntegerObj input10
    ~?= Right True
    , "testEvalIntegerExpression 10 / integer value" ~: _intValue input10 ~?= 0
    , "testEvalIntegerExpression 11 / Is object integer?"
    ~:  isIntegerObj input11
    ~?= Right True
    , "testEvalIntegerExpression 11 / integer value" ~: _intValue input11 ~?= 60
    , "testEvalIntegerExpression 12 / Is object integer?"
    ~:  isIntegerObj input12
    ~?= Right True
    , "testEvalIntegerExpression 12 / integer value" ~: _intValue input12 ~?= 30
    , "testEvalIntegerExpression 13 / Is object integer?"
    ~:  isIntegerObj input13
    ~?= Right True
    , "testEvalIntegerExpression 13 / integer value" ~: _intValue input13 ~?= 37
    , "testEvalIntegerExpression 14 / Is object integer?"
    ~:  isIntegerObj input14
    ~?= Right True
    , "testEvalIntegerExpression 14 / integer value" ~: _intValue input14 ~?= 37
    , "testEvalIntegerExpression 15 / Is object integer?"
    ~:  isIntegerObj input15
    ~?= Right True
    , "testEvalIntegerExpression 15 / integer value" ~: _intValue input15 ~?= 50
    ]
  where
    input5  = "5 + 5 + 5 + 5 - 10"
    input6  = "2 * 2 * 2 * 2 * 2"
    input7  = "-50 + 100 + -50"
    input8  = "5 * 2 + 10"
    input9  = "5 + 2 * 10"
    input10 = "20 + 2 * -10"
    input11 = "50 / 2 * 2 + 10"
    input12 = "2 * (5 + 10)"
    input13 = "3 * 3 * 3 + 10"
    input14 = "3 * (3 * 3) + 10"
    input15 = "(5 + 10 * 2 + 15 / 3) * 2 + -10"
    isIntegerObj s = case _object s of
        (Obj.Integer _) -> Right True
        x               -> Left x

-- | testEvalBooleanExpression
--
testEvalBooleanExpression :: Test
testEvalBooleanExpression = TestList
    [ "testEvalBooleanExpression 1 / Is object Boolean?"
    ~:  isBoolObj input1
    ~?= Right True
    , "testEvalBooleanExpression 1 / Boolean value"
    ~:  _boolValue input1
    ~?= True
    , "testEvalBooleanExpression 2 / Is object Boolean?"
    ~:  isBoolObj input2
    ~?= Right True
    , "testEvalBooleanExpression 2 / Boolean value"
    ~:  _boolValue input2
    ~?= False
    , "testEvalBooleanExpression 3 / Is object Boolean?"
    ~:  isBoolObj input3
    ~?= Right True
    , "testEvalBooleanExpression 3 / Boolean value"
    ~:  _boolValue input3
    ~?= True
    , "testEvalBooleanExpression 4 / Is object Boolean?"
    ~:  isBoolObj input4
    ~?= Right True
    , "testEvalBooleanExpression 4 / Boolean value"
    ~:  _boolValue input4
    ~?= False
    , "testEvalBooleanExpression 5 / Is object Boolean?"
    ~:  isBoolObj input5
    ~?= Right True
    , "testEvalBooleanExpression 5 / Boolean value"
    ~:  _boolValue input5
    ~?= False
    , "testEvalBooleanExpression 6 / Is object Boolean?"
    ~:  isBoolObj input6
    ~?= Right True
    , "testEvalBooleanExpression 6 / Boolean value"
    ~:  _boolValue input6
    ~?= False
    , "testEvalBooleanExpression 7 / Is object Boolean?"
    ~:  isBoolObj input7
    ~?= Right True
    , "testEvalBooleanExpression 7 / Boolean value"
    ~:  _boolValue input7
    ~?= True
    , "testEvalBooleanExpression 8 / Is object Boolean?"
    ~:  isBoolObj input8
    ~?= Right True
    , "testEvalBooleanExpression 8 / Boolean value"
    ~:  _boolValue input8
    ~?= False
    , "testEvalBooleanExpression 9 / Is object Boolean?"
    ~:  isBoolObj input9
    ~?= Right True
    , "testEvalBooleanExpression 9 / Boolean value"
    ~:  _boolValue input9
    ~?= False
    , "testEvalBooleanExpression 10 / Is object Boolean?"
    ~:  isBoolObj input10
    ~?= Right True
    , "testEvalBooleanExpression 10 / Boolean value"
    ~:  _boolValue input10
    ~?= True
    , "testEvalBooleanExpression 11 / Is object Boolean?"
    ~:  isBoolObj input11
    ~?= Right True
    , "testEvalBooleanExpression 11 / Boolean value"
    ~:  _boolValue input11
    ~?= True
    , "testEvalBooleanExpression 12 / Is object Boolean?"
    ~:  isBoolObj input12
    ~?= Right True
    , "testEvalBooleanExpression 12 / Boolean value"
    ~:  _boolValue input12
    ~?= True
    , "testEvalBooleanExpression 13 / Is object Boolean?"
    ~:  isBoolObj input13
    ~?= Right True
    , "testEvalBooleanExpression 13 / Boolean value"
    ~:  _boolValue input13
    ~?= False
    , "testEvalBooleanExpression 14 / Is object Boolean?"
    ~:  isBoolObj input14
    ~?= Right True
    , "testEvalBooleanExpression 14 / Boolean value"
    ~:  _boolValue input14
    ~?= True
    , "testEvalBooleanExpression 15 / Is object Boolean?"
    ~:  isBoolObj input15
    ~?= Right True
    , "testEvalBooleanExpression 15 / Boolean value"
    ~:  _boolValue input15
    ~?= True
    , "testEvalBooleanExpression 16 / Is object Boolean?"
    ~:  isBoolObj input16
    ~?= Right True
    , "testEvalBooleanExpression 16 / Boolean value"
    ~:  _boolValue input16
    ~?= True
    , "testEvalBooleanExpression 17 / Is object Boolean?"
    ~:  isBoolObj input17
    ~?= Right True
    , "testEvalBooleanExpression 17 / Boolean value"
    ~:  _boolValue input17
    ~?= False
    , "testEvalBooleanExpression 18 / Is object Boolean?"
    ~:  isBoolObj input18
    ~?= Right True
    , "testEvalBooleanExpression 18 / Boolean value"
    ~:  _boolValue input18
    ~?= False
    , "testEvalBooleanExpression 19 / Is object Boolean?"
    ~:  isBoolObj input19
    ~?= Right True
    , "testEvalBooleanExpression 19 / Boolean value"
    ~:  _boolValue input19
    ~?= True
    ]
  where
    input1  = "true"
    input2  = "false"
    input3  = "1 < 2"
    input4  = "1 > 2"
    input5  = "1 < 1"
    input6  = "1 > 1"
    input7  = "1 == 1"
    input8  = "1 != 1"
    input9  = "1 == 2"
    input10 = "1 != 2"
    input11 = "true == true"
    input12 = "false == false"
    input13 = "true == false"
    input14 = "true != false"
    input15 = "false != true"
    input16 = "(1 < 2) == true"
    input17 = "(1 < 2) == false"
    input18 = "(1 > 2) == true"
    input19 = "(1 > 2) == false"

    isBoolObj s = case _object s of
        (Obj.Boolean _) -> Right True
        x               -> Left x

-- | testBangOperator
--
testBangOperator :: Test
testBangOperator = TestList
    [ "testBangOperator 1" ~: _boolValue "!true" ~?= False
    , "testBangOperator 2" ~: _boolValue "!false" ~?= True
    , "testBangOperator 3" ~: _boolValue "!5" ~?= False
    , "testBangOperator 4" ~: _boolValue "!!true" ~?= True
    , "testBangOperator 5" ~: _boolValue "!!false" ~?= False
    , "testBangOperator 6" ~: _boolValue "!!5" ~?= True
    ]



-- | testIfElseExpressions
--
testIfElseExpressions :: Test
testIfElseExpressions = TestList
    [ "testIfElseExpressions 1" ~: _evalObject "if (true) { 10 }" ~?= ObInt
        10
    , "testIfElseExpressions 2"
    ~:  _evalObject "if (false) { 10 }"
    ~?= ObNull
    , "testIfElseExpressions 3" ~: _evalObject "if (1) { 10 }" ~?= ObInt 10
    , "testIfElseExpressions 4"
    ~:  _evalObject "if (1 < 2) { 10 }"
    ~?= ObInt 10
    , "testIfElseExpressions 5"
    ~:  _evalObject "if (1 > 2) { 10 }"
    ~?= ObNull
    , "testIfElseExpressions 6"
    ~:  _evalObject "if (1 > 2) { 10 } else { 20 }"
    ~?= ObInt 20
    , "testIfElseExpressions 7"
    ~:  _evalObject "if (1 < 2) { 10 } else { 20 }"
    ~?= ObInt 10
    ]



-- | testReturnStatements
--
testReturnStatements :: Test
testReturnStatements = TestList
    [ "testReturnStatements 1" ~: _evalObject "return 10;" ~?= ObInt 10
    , "testReturnStatements 2" ~: _evalObject "return 10; 9;" ~?= ObInt 10
    , "testReturnStatements 3" ~: _evalObject "return 2 * 5; 9;" ~?= ObInt 10
    , "testReturnStatements 4" ~: _evalObject "9; return 2 * 5; 9;" ~?= ObInt 10
    , "testReturnStatements 5" ~: _evalObject test5 ~?= ObInt 10
    ]
  where
    test5 = [r|
if (10 > 1) {
    if (10 > 1) {
      return 10;
    }

    return 1;
}
|]



-- | testErrorHandling
--
testErrorHandling :: Test
testErrorHandling = TestList
    [ "testErrorHandling 1" ~: _evalObject "5 + true;" ~?= ObErr "type mismatch: INTEGER + BOOLEAN"
    , "testErrorHandling 2" ~: _evalObject "5 + true; 5;" ~?= ObErr "type mismatch: INTEGER + BOOLEAN"
    , "testErrorHandling 3" ~: _evalObject "-true" ~?= ObErr "unknown operator: -BOOLEAN"
    , "testErrorHandling 4" ~: _evalObject "true + false" ~?= ObErr "unknown operator: BOOLEAN + BOOLEAN"
    , "testErrorHandling 5" ~: _evalObject "5; true + false; 5" ~?= ObErr "unknown operator: BOOLEAN + BOOLEAN"
    , "testErrorHandling 6" ~: _evalObject "if (10 > 1) { true + false; }" ~?= ObErr "unknown operator: BOOLEAN + BOOLEAN"
    , "testErrorHandling 7" ~: _evalObject test7 ~?= ObErr "unknown operator: BOOLEAN + BOOLEAN"
    , "testErrorHandling 8" ~: _evalObject "foobar" ~?= ObErr "identifier not found: foobar"
    , "testErrorHandling 9" ~: _evalObject test9 ~?= ObErr "unknown operator: STRING_OBJ - STRING_OBJ"
    ]
  where
    test7 = [r|
if (10 > 1) {
    if (10 > 1) {
      return true + false;
    }

    return 1;
}
|]
    test9 = [r|"Hello" - "World!"|]


-- | testLetStatement
--
testLetStatement :: Test
testLetStatement = TestList
    [ "testLetStatement 1" ~: _evalObject "let a = 5; a;" ~?= ObInt 5
    , "testLetStatement 2" ~: _evalObject "let a = 5 * 5; a;" ~?= ObInt 25
    , "testLetStatement 3" ~: _evalObject "let a = 5; let b = a; b;" ~?= ObInt 5
    , "testLetStatement 4" ~: _evalObject "let a = 5; let b = a; let c = a + b + 5; c;" ~?= ObInt 15
    ]

-- | testFunctionObject
--
testFunctionObject :: Test
testFunctionObject = TestList
    [ "Is function obj? 1" ~: (isFunctionObj . _object) input1 ~?= Right True
    , "params length 1" ~: (length . Obj.parameters . _object) input1 ~?= 1
    , "params contents 1" ~: (Ast.string . head . Obj.parameters . _object) input1 ~?= "x"
    , "body contents 1" ~: (Ast.string . Obj.body . _object) input1 ~?= "(x + 2)"
    ]
  where
    input1 = "fn(x) { x + 2; };"
    isFunctionObj Obj.Function{} = Right True
    isFunctionObj o              = Left $ Obj.getObjectType o


-- | testFunctionApplication
--
testFunctionApplication :: Test
testFunctionApplication = TestList
    [ "function application 1" ~: _evalObject input1 ~?= ObInt 5
    , "function application 2" ~: _evalObject input2 ~?= ObInt 5
    , "function application 3" ~: _evalObject input3 ~?= ObInt 10
    , "function application 4" ~: _evalObject input4 ~?= ObInt 10
    , "function application 5" ~: _evalObject input5 ~?= ObInt 20
    , "function application 6" ~: _evalObject input6 ~?= ObInt 5
    , "closure 1" ~: _evalObject inputClosure1 ~?= ObInt 4
    , "closure 2" ~: _evalObject inputClosure2 ~?= ObInt 155
    ]
  where
    input1 = "let identity = fn(x) { x; }; identity(5);"
    input2 = "let identity = fn(x) { return x; }; identity(5);"
    input3 = "let double = fn(x) { x * 2; }; double(5);"
    input4 = "let add = fn(x, y) { x + y; }; add(5, 5);"
    input5 = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));"
    input6 = "fn(x) { x; }(5)"

    inputClosure1 = [r|
let newAdder = fn(x) {
  fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
|]

    inputClosure2 = [r|
let add = fn(a, b) { a + b };
let applyFunc = fn(a, b, func) { func(a, b) };
applyFunc(100, 55, add);
|]

    assertEnv Obj.NothingEnv        = []
    assertEnv (Obj.Environment s o) = M.keys s ++ assertEnv o

    unwrap (Right e) = e
    unwrap (Left e)  = e

-- | testStringLiteral
--
testStringLiteral :: Test
testStringLiteral = TestList
    [ "string literal 1" ~: _evalObject input1 ~?= ObStr "Hello World"
    ]
  where
    input1 = [r|"Hello World"|]

-- | testStringConcatenation
--
testStringConcatenation :: Test
testStringConcatenation = TestList
    [ "string literal 1" ~: _evalObject input1 ~?= ObStr "Hello World!"
    ]
  where
    input1 = [r|"Hello" + " " + "World!"|]


-- | testBuiltinFunctions
--
testBuiltinFunctions :: Test
testBuiltinFunctions = TestList
    [ "len 0 letter word" ~: _evalObject input1 ~?= ObInt 0
    , "len 4 letter word" ~: _evalObject input2 ~?= ObInt 4
    , "len hello world" ~: _evalObject input3 ~?= ObInt 11
    , "len number" ~: _evalObject input4 ~?= ObErr "argument to `len` not supported, got INTEGER"
    , "len two arguments" ~: _evalObject input5 ~?= ObErr "wrong number of arguments. got=2, want=1"
    , "len 3 array literal" ~: _evalObject input6 ~?= ObInt 3
    , "len 3 array identifire" ~: _evalObject input7 ~?= ObInt 5
    , "first 1" ~: _evalObject input8 ~?= ObStr "hoge"
    , "first 2" ~: _evalObject input9 ~?= ObInt 100
    , "first string" ~: _evalObject input10 ~?= ObErr "argument to `first` must be ARRAY, got STRING_OBJ"
    , "first no elements" ~: _evalObject input11 ~?= ObNull
    , "last 1" ~: _evalObject inputLast1 ~?= ObInt 3
    , "last empty list" ~: _evalObject inputLast2 ~?= ObNull
    , "last string" ~: _evalObject inputLast3 ~?= ObErr "argument to `last` must be ARRAY, got STRING_OBJ"
    , "last few arguments" ~: _evalObject inputLast4 ~?= ObErr "wrong number of arguments. got=3, want=1"
    , "rest 1" ~: _evalObject inputRest1 ~?= ObArray [ObInt 2,ObInt 3,ObInt 4,ObInt 5]
    , "rest few arguments" ~: _evalObject inputRest2 ~?= ObErr "wrong number of arguments. got=2, want=1"
    , "rest string" ~: _evalObject inputRest3 ~?= ObErr "argument to `rest` must be ARRAY, got STRING_OBJ"
    , "rest 2" ~: _evalObject inputRest4 ~?= ObArray [ObInt 6,ObInt 7]
    , "rest empty list" ~: _evalObject inputRest5 ~?= ObNull
    , "push 1" ~: _evalObject inputPush1 ~?= ObArray [ObInt 1, ObInt 2, ObInt 3, ObInt 4, ObInt 5]
    , "push 2" ~: _evalObject inputPush2 ~?= ObArray [ObInt 1, ObInt 2, ObInt 3]
    , "push 3" ~: _evalObject inputPush3 ~?= ObArray [ObInt 1, ObInt 2, ObInt 3, ObInt 5]
    , "push 4" ~: _evalObject inputPush4 ~?= ObArray [ObStr "hoge"]
    , "push not 2 arguments 1" ~: _evalObject inputPush5 ~?= ObErr "wrong number of arguments. got=1, want=2"
    , "push not 2 arguments 2" ~: _evalObject inputPush6 ~?= ObErr "wrong number of arguments. got=0, want=2"
    ]
  where
    input1 = [r|len("")|]
    input2 = [r|len("four")|]
    input3 = [r|len("hello world")|]
    input4 = [r|len(1)|]
    input5 = [r|len("one", "two")|]
    input6 = [r|len([1, 2, 3])|]
    input7 = [r|let myArray = [1, 2, 3, 4, 5];len(myArray);|]
    input8 = [r|first(["hoge", 3, "fuga"])|]
    input9 = [r|first([100])|]
    input10 = [r|first("abcde")|]
    input11 = [r|first([])|]
    inputLast1 = [r|last([1,2,3])|]
    inputLast2 = [r|last([])|]
    inputLast3 = [r|last("hoge")|]
    inputLast4 = [r|last([5,6,7], 125, "hoge")|]
    inputRest1 = [r|rest([1,2,3,4,5])|]
    inputRest2 = [r|rest([1,2,3,4,5], "hoge")|]
    inputRest3 = [r|rest("string")|]
    inputRest4 = [r|let arr = [5, 6, 7];rest(arr);|]
    inputRest5 = [r|rest([])|]
    inputPush1 = [r|push([1, 2, 3, 4], 5)|]
    inputPush2 = [r|let a = [1,2,3]; let b = push(a, 5); a;|]
    inputPush3 = [r|let a = [1,2,3]; let b = push(a, 5); b;|]
    inputPush4 = [r|push([], "hoge")|]
    inputPush5 = [r|push([1,2,3])|]
    inputPush6 = [r|push()|]

-- | testArrayLiterals
--
testArrayLiterals :: Test
testArrayLiterals = TestList
    [ "test array literals 1" ~: _evalObject input1 ~?= ObArray [ObInt 1, ObInt 4, ObInt 6]
    ]
  where
    input1 = "[1, 2 * 2, 3 + 3]"


-- | testArrayIndexExpressions
--
testArrayIndexExpressions :: Test
testArrayIndexExpressions = TestList
    [ "test array index expressions 1" ~: _evalObject "[1, 2, 3][0]" ~?= ObInt 1
    , "test array index expressions 2" ~: _evalObject "[1, 2, 3][1]" ~?= ObInt 2
    , "test array index expressions 3" ~: _evalObject "[1, 2, 3][2]" ~?= ObInt 3
    , "test array index expressions 4" ~: _evalObject "let i = 0; [1][i]" ~?= ObInt 1
    , "test array index expressions 5" ~: _evalObject "[1, 2, 3][1 + 1]" ~?= ObInt 3
    , "test array index expressions 6" ~: _evalObject "let myArray = [1, 2, 3]; myArray[2]" ~?= ObInt 3
    , "test array index expressions 7" ~: _evalObject "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]" ~?= ObInt 6
    , "test array index expressions 8" ~: _evalObject "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]" ~?= ObInt 2
    , "test array index expressions 9" ~: _evalObject "[1, 2, 3][3]" ~?= ObNull
    , "test array index expressions 10" ~: _evalObject "[1, 2, 3][-1]" ~?= ObNull
    ]


-- | testBuiltinDrive
--
testBuiltinDrive :: Test
testBuiltinDrive = TestList
    [ --"test program sample function" ~: _evalObject inputSample ~?= ObInt 101
    --, "test program map function" ~: _evalObject inputMap ~?= ObArray [ObInt 2, ObInt 4, ObInt 6, ObInt 8]
    ]
  where
    inputSample = [r| let add = fn(arg) { fn(arg2) { arg2 + 200; }; 100; }; add(1); |]
-- map function source
    inputMap = [r|
let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
        }
    };

    iter(arr, []);
};

let a = [1, 2, 3, 4];
let double = fn(x) { x * 2 };
map(a, double);
|]
