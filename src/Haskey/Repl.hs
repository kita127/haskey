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
prompt :: Handle -> IO ()
prompt hOut = do
    hPutStr hOut ">> "
    hFlush hOut
    return ()

-- | start
--
-- TODO:
-- ファイルハンドルは main が任意のものを渡し、start は標準入出力以外にも
-- 対応できるようにする
--
start :: Handle -> Handle -> IO ()
start hIn hOut = do
    greet hOut
    loop hIn hOut Obj.newEnvironment

-- | loop
--
loop :: Handle -> Handle -> Obj.Environment -> IO ()
loop hIn hOut e = do
    prompt hOut
    l <- TIO.hGetLine hIn
    let (res, newEnv) = unwrapInterpreted e $ interprete l e
    TIO.hPutStr hOut res
    loop hIn hOut newEnv

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
run prg e = case Evl.runEvaluator (Evl.eval prg) e of
    (Evl.Done o e') -> do
        let out =
                if o /= Obj.Void then Obj.inspect o <> "\n" else Obj.inspect o
        return (out, e')
    (Evl.Error o _) -> Left (Obj.inspect o <> "\n")

-- | checkSyntax
--
checkSyntax :: Ast.Program -> Either T.Text Ast.Program
checkSyntax prg = if hasError prg
    then Left (chobiFace <> "\n" <> errContents <> "\n")
    else Right prg
  where
    errContents =
        T.intercalate "\t" . map Ast.string . Ast.extractFailers $ prg

greet :: Handle -> IO ()
greet hOut = TIO.hPutStrLn hOut greeting

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
