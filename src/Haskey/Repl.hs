{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Haskey.Repl
    ( start
    )
where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Haskey.Ast                    as Ast
import           Haskey.Evaluator              as Evl
import qualified Haskey.Lexer                  as Lex
import           Haskey.Object                 as Obj
import qualified Haskey.Parser                 as Prs
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

-- | loop
--
loop :: Obj.Environment -> IO ()
loop e = do
    prompt
    l <- TIO.getLine
    let (out, newEnv) = unwrapInterpreted e $ interprete l e
    TIO.putStr out
    loop newEnv

-- | unwrapInterpreted
--
unwrapInterpreted
    :: Obj.Environment
    -> Either T.Text (T.Text, Obj.Environment)
    -> (T.Text, Obj.Environment)
unwrapInterpreted e (Left  s  ) = (s, e)
unwrapInterpreted _ (Right res) = res

-- | interprete
--
interprete
    :: T.Text -> Obj.Environment -> Either T.Text (T.Text, Obj.Environment)
interprete s e = do
    let prg = Prs.parse $ Lex.lexicalize s
    checkSyntax prg
    run prg e

-- | run
--
run :: Ast.Program -> Obj.Environment -> Either T.Text (T.Text, Obj.Environment)
run prg e = do
    let (object, newEnv) = case Evl.runEvaluator (Evl.eval prg) e of
            (Evl.Done  o e') -> (o, e')
            (Evl.Error o e') -> (o, e')
        out = if object /= Obj.Void
            then Obj.inspect object <> "\n"
            else Obj.inspect object
    return (out, newEnv)

-- | checkSyntax
--
checkSyntax :: Ast.Program -> Either T.Text Ast.Program
checkSyntax prg = if hasError prg
    then Left (chobiFace <> "\n" <> errContents <> "\n")
    else Right prg
  where
    errContents =
        T.intercalate "\t" . map Ast.string . Ast.extractFailers $ prg

greet :: IO ()
greet = TIO.putStrLn greeting

-- | hasError
--
hasError :: Ast.Program -> Bool
hasError = not . null . Ast.extractFailers



-- | greeting
--
greeting :: T.Text
greeting = [r|Hello! This is the Haskey programming language!
Feel free to type in commands
Usage: haskey --help|]

-- | chobiFace
--
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
