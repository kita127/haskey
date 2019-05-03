{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Text         as T
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList
      [ testSample
      ]
    return ()

testSample :: Test
testSample = TestList
  [ "eval testSample test 1" ~:
        "hello test" ~?= "hello test"
  ]
