{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Haskey.Repl (
  start
) where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Haskey.Ast        as Ast
import           Haskey.Evaluator  as Evl
import           Haskey.Lexer
import           Haskey.Object     as Obj
import           Haskey.Parser
import qualified Haskey.Token      as Tok
import           Text.RawString.QQ

prompt = ">> "

-- | start
--
-- TODO:
-- ファイルハンドルは main が任意のものを渡し、start は標準入出力以外にも
-- 対応できるようにする
--
-- TODO:
-- '>>' と同じ行に入力がされない。改行されてから入力になってしまう
-- Haskell の IO アクションが関数の戻り値評価時に実行されるからみで起きる減少
--
start :: IO ()
start = do
    putStrLn prompt
    l <- TIO.getLine
    let prg = (parse . lexicalize) l

    if hasError prg
    then do
        printParseError prg
        start
    else do
        case Evl.runEvalutor (Evl.eval prg) Evl.newEnvironment of
            (Evl.Done obj _ ) -> TIO.putStrLn $ Obj.inspect obj
            (Evl.Error obj)   -> TIO.putStrLn $ Obj.inspect obj
        start


-- | hasError
--
hasError :: Ast.Program -> Bool
hasError = not . null . Ast.extractFailers




-- | printParseError
--
printParseError :: Ast.Program -> IO ()
printParseError prg = do
    TIO.putStrLn chobiFace
    mapM_ (TIO.putStrLn . ("\t" <>) . Ast.string) . Ast.extractFailers $ prg

chobiFace :: T.Text
chobiFace = [r|
　　　　　　 r‐..、　　　　 　　　　　　　　　　　　,,,,
　　　　　　 |;;'ヾ;＼　　 　　　　　　　　　 　 ,r'.:;;'`ヽ
　　　　　　 |;!　　ﾞ､;;;:､ ,. .:.:;:;.:.:. 、ｨ;:;;.:'´ 　 ﾞ;|
　　　　　　 i!　ﾄ､　';;:;;:;;:;:;:;;::;;:;:;:;,. '´ ｲ:.　　;|
　　　　　　 i!　|.:.レ',:;;;,r'''从;:;;;;;;;;;:;:;,. '´　　j!.::　　 l
　　　　　　 i!　i,.:;;;;;;,'′ 　 ,'.:;:;;;;;;く　　 ノ.:.:. 　 |
　　　　　　 i!,.'ﾞ;:;;;;;,′　　 i.:.:;:;:;;:;;ﾞ､,,　f.:.:.: 　|
　　　 　 　 l;;;;;;;;:;:;′ 　 　l;:;:;:;;;;;;;;;:;;:.`ヽ.:.:　　 ,'1
　　　 　 　 l;:;:;;' ｀`　　　　 ﾚ'´`ヾ;;;;;;;;;;;;;;:;.＼　 　,';:!
　　　　　　 |;:;:ﾄ､ 　,　 　 　 　 　 };:;;;;;;;;;;;;;;;;;;;:;:`ｰ':;;;|
　　　　　　 i　;;(・);;;　　　､, _ ,,.:;:;;:;;｀, ｀ヾ;;;r''´ヾ;:;:;;;;L
　　　　　　 i　;;;:.''ﾞ´ 　 　 ヾ;:;;(･);;;;;;,'　　　　　　i;:;:;:;:;:;.｀ヽ
　　　 　 　　V　　　　 　　 　`ヾ;;;;:ｼ　　　　　　ﾉ.::;;;;:;:;:;:;:;:;.､
　　 　 　 　 ,′　　　　　　　　　　　　　 　 　　!.:;:;:;:;:;:;:;:;;;;:;:;
　　 　 　_ -i　　　　　　　　　　　　　　　　　　,'.::;:;;:;;;;;;;;;;;;;;;;;;
　　　 - ─.|　 にﾆ>;;,　 　 　 　､ ,′.ﾞ　　　, '.:.:;:;:;;;;;;;;;;;;;;;;;;;;
　　　　　"´ﾞ､　ヾ;;;;; 'ﾞ　　　 ‐ ､,.ｲヽ｀　 ,..ィ.:.:;:;:;;;;;;;;;;;;;;;;;;;;;;;;
　　　　　　　 ゝ,,;;入;;､,, _＿ ノ´｀''''''"´`　｀ﾞﾞﾞﾞ"´ ｀;:;;:;:;:;;;;;;;;
　　　　　　　 ﾉ　　　　 ｀ ''` ｀　　　　　　　　　　 .:.:;:;:;;;;;;;;;;;;
　　　　　 　 ,ﾞ　　　　　　　　　　　　　　　　　　　　. ::;:;:;;;;;;;;
　　 　 　　 ,′　　　　　　　　　　　　　　　　　　　　 .::;:;;;;;;;; --

    あのぅ、間違えてると思います・・・

|]
