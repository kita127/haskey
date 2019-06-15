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
import           System.IO
import           Text.RawString.QQ

-- | prompt
--
-- 出力は改行が来るまでバッファリングされるため
-- hFlush する必要がある
--
prompt :: IO ()
prompt = do
    putStr ">> "
    hFlush stdout
    return ()



-- | start
--
-- TODO:
-- ファイルハンドルは main が任意のものを渡し、start は標準入出力以外にも
-- 対応できるようにする
--
start :: IO ()
start = do
    greet
    loop Obj.newEnvironment
    where
      loop env = do
          prompt
          l <- TIO.getLine
          let prg = (parse . lexicalize) l

          if hasError prg
          then do
              printParseError prg
              loop env
          else do
              let (obj, env') = (case Evl.runEvaluator (Evl.eval prg) env of
                                      (Evl.Done obj env' ) -> (obj, env')
                                      (Evl.Error obj env') -> (obj, env'))
              if obj /= Obj.Void
              then TIO.putStrLn $ Obj.inspect obj
              else return ()
              loop env'


greet :: IO ()
greet = TIO.putStrLn greeting

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

greeting :: T.Text
greeting = [r|Hello! This is the Haskey programming language!
Feel free to type in commands
Usage: haskey --help|]

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
