{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Haskey.Ast                    as Ast
import qualified Haskey.Evaluator              as Evl
import qualified Haskey.Lexer                  as Lex
import qualified Haskey.Object                 as Obj
import qualified Haskey.Parser                 as Prs
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList [testSample]
    return ()



-- | testEvalIntegerExpression
--
testSample :: Test
testSample = TestList ["test sample" ~: "5" ~?= "5"]
  where
