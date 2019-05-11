{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Text                     as T
import qualified Haskey.Ast                    as Ast
import qualified Haskey.Evaluator              as Eval
import qualified Haskey.Lexer                  as Lex
import qualified Haskey.Object                 as Obj
import qualified Haskey.Parser                 as Prs
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
        ]
    return ()


data Ob = ObInt Integer | ObNull | ObErr String
    deriving (Eq, Show)

-- | support
--
_program :: T.Text -> Ast.Program
_program = Prs.parse . Lex.lexicalize

_object :: T.Text -> Obj.Object
_object = Eval.eval . _program

_boolValue :: T.Text -> Bool
_boolValue = Obj.boolVal . _object

_intValue :: T.Text -> Integer
_intValue = Obj.intVal . _object


_evalObject :: T.Text -> Ob
_evalObject s = case _object s of
        Obj.Null      -> ObNull
        Obj.Integer v -> ObInt v
        o             -> ObErr $ show $ Obj.getObjectType o

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
