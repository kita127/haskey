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

-- | testEvalIntegerExpression
--
testEvalIntegerExpression :: Test
testEvalIntegerExpression = TestList
  [ "testEvalIntegerExpression / Is object integer?" ~: _object "5" ~?=
        Obj.Integer {
          Obj.intVal = 5
        }
  , "testEvalIntegerExpression / integer value" ~: (Obj.intVal . _object) "5" ~?= 5

  ]
