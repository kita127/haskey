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
    runTestTT $ TestList
        [testSample, testAdvanced, testMapFunction, testReduceAndSum]
    return ()


-- | helper
--
tEval :: T.Text -> Either String Obj.Object
tEval s = case Evl.runEvaluator (e s) (Obj.newEnvironment, "") of
    (Evl.Done obj _ _) -> Right obj
    (Evl.Error err _ ) -> Left $ show err
    where e = Evl.eval . Prs.parse . Lex.lexicalize

tArrayObject :: Obj.Object -> Either String Obj.Object
tArrayObject o@Obj.Array{} = Right o
tArrayObject x               = Left $ show x

tInteger :: Obj.Object -> Either String Integer
tInteger (Obj.Integer v) = Right v
tInteger x               = Left $ show x


-- | testSample
--
testSample :: Test
testSample = TestList ["test sample" ~: "5" ~?= "5"]

-- | testAdvanced
--
testAdvanced :: Test
testAdvanced = TestList ["test addvanced " ~: helper input1 ~?= Right 0]
  where
    input1 = [r|
let funca = fn(a) {
    if (a < 0) {
        return 0;
    } else {
        funca(a - 1);
    }
};
funca(10);
|]

    helper s = do
        o <- tEval s
        tInteger o

-- | testMapFunction
--
testMapFunction :: Test
testMapFunction = TestList
    ["test map function" ~: helper input1 ~?= Right [2, 4, 6, 8]]
  where
    input1 = [r|
let map = fn(arr, f) {
    let iter = fn(arr, accumlated) {
        if (len(arr) == 0) {
            accumlated
        } else {
            iter(rest(arr), push(accumlated, f(first(arr))));
        }
    };

    iter(arr, []);
};
let a = [1, 2, 3, 4];
let double = fn(x) { x * 2 };
map(a, double);
|]

    helper s = do
        o   <- tEval s
        arr <- tArrayObject o
        mapM tInteger $ Obj.elements arr


-- | testReduceAndSum
--
testReduceAndSum :: Test
testReduceAndSum = TestList
    ["test reduce and sum function" ~: helper input1 ~?= Right 15]
  where
    input1 = [r|
let reduce = fn(arr, initial, f) {
    let iter = fn(arr, result) {
        if (len(arr) == 0) {
            result
        } else {
            iter(rest(arr), f(result, first(arr)));
        }
    };
    iter(arr, initial);
};

let sum = fn(arr) {
    reduce(arr, 0, fn(initial, el) { initial + el });
};

sum([1, 2, 3, 4, 5]);
|]

    helper s = do
        o <- tEval s
        tInteger o
