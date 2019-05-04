{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Text         as T
import qualified Haskey.Ast        as Ast
import qualified Haskey.Evaluator  as Eval
import qualified Haskey.Lexer      as Lex
import qualified Haskey.Object     as Obj
import qualified Haskey.Parser     as Prs
import qualified Haskey.Token      as Tok
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList
      [ testSample
      , testEvalIntegerExpression
      , testEvalBooleanExpression
      , testBangOperator
      ]
    return ()

testSample :: Test
testSample = TestList
  [ "eval testSample test 1" ~:
        "hello test" ~?= "hello test"
  ]


-- | support
--
_program = Prs.parse . Lex.lexicalize

_object = Eval.eval . _program

_boolValue = Obj.boolVal . _object

-- | testEvalIntegerExpression
--
testEvalIntegerExpression :: Test
testEvalIntegerExpression = TestList
  [ "testEvalIntegerExpression 1 / Is object integer?" ~: isIntegerObj "5" ~?= Right True

  , "testEvalIntegerExpression 1 / integer value" ~: _intValue "5" ~?= 5

  , "testEvalIntegerExpression 2 / Is object integer?" ~: isIntegerObj "10" ~?= Right True

  , "testEvalIntegerExpression 2 / integer value" ~: _intValue "10" ~?= 10

  , "testEvalIntegerExpression 3 / Is object integer?" ~: isIntegerObj "-5" ~?= Right True

  , "testEvalIntegerExpression 3 / integer value" ~: _intValue "-5" ~?= -5

  , "testEvalIntegerExpression 4 / Is object integer?" ~: isIntegerObj "-10" ~?= Right True

  , "testEvalIntegerExpression 4 / integer value" ~: _intValue "-10" ~?= -10


  ]
  where
    _intValue = Obj.intVal . _object
    isIntegerObj s = case _object s of
                        (Obj.Integer _) -> Right True
                        x               -> Left x

-- | testEvalBooleanExpression
--
testEvalBooleanExpression :: Test
testEvalBooleanExpression = TestList
  [ "testEvalBooleanExpression / Is object integer?" ~: _object input1 ~?=
        Obj.Boolean { Obj.boolVal = True }

  , "testEvalBooleanExpression / integer value" ~: _boolValue input1 ~?= True

  , "testEvalBooleanExpression / Is object integer?" ~: _object input2 ~?=
        Obj.Boolean { Obj.boolVal = False }

  , "testEvalBooleanExpression / integer value" ~: _boolValue input2 ~?= False


  ]
  where
    input1 = "true"
    input2 = "false"

-- | testBangOperator
--
testBangOperator :: Test
testBangOperator = TestList
  [ "testBangOperator 1" ~: _boolValue "!true"   ~?= False
  , "testBangOperator 2" ~: _boolValue "!false"  ~?= True
  , "testBangOperator 3" ~: _boolValue "!5"      ~?= False
  , "testBangOperator 4" ~: _boolValue "!!true"  ~?= True
  , "testBangOperator 5" ~: _boolValue "!!false" ~?= False
  , "testBangOperator 6" ~: _boolValue "!!5"     ~?= True
  ]

