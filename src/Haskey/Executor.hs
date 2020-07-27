{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Haskey.Executor
    ( repl
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


type RetEnv = (T.Text, Obj.Environment, Evl.Buffer)

-- | repl
--
repl :: Handle -> Handle -> Handle -> IO ()
repl hIn hOut hErr = do
    greet hOut
    loop hIn hOut hErr Obj.newEnvironment

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

-- | loop
--
loop :: Handle -> Handle -> Handle -> Obj.Environment -> IO ()
loop hIn hOut hErr e = do
    prompt hOut
    l <- TIO.hGetLine hIn
    let res = interprete l e
    putResult hOut hErr res
    loop hIn hOut hErr $ fetchEnv res

-- | interprete
--
interprete :: T.Text -> Obj.Environment -> Either RetEnv RetEnv
interprete s e = do
    let prg = Prs.parse $ Lex.lexicalize s
    checkSyntax prg e
    run prg e ""

-- | run
--
run :: Ast.Program -> Obj.Environment -> Evl.Buffer -> Either RetEnv RetEnv
run prg e b = case Evl.runEvaluator (Evl.eval prg) (e, b) of
    (Evl.Done o e' b') -> do
        let res =
                if o /= Obj.Void then Obj.inspect o <> "\n" else Obj.inspect o
        return (res, e', b')
    (Evl.Error o e') -> Left (Obj.inspect o <> "\n", e', b)

-- | checkSyntax
--
checkSyntax :: Ast.Program -> Obj.Environment -> Either RetEnv Ast.Program
checkSyntax prg e = if hasError prg
    then Left (chobiFace <> "\n" <> errContents <> "\n", e, "")
    else Right prg
  where
    errContents =
        T.intercalate "\t" . map Ast.string . Ast.extractFailers $ prg

-- | putResult
--
putResult :: Handle -> Handle -> Either RetEnv RetEnv -> IO ()
putResult hOut _ (Right (res, _, b)) = bufFlush hOut b >> TIO.hPutStr hOut res
putResult _ hErr (Left (res, _, _)) = TIO.hPutStr hErr res

-- | bufFlush
--
bufFlush :: Handle -> Buffer -> IO ()
bufFlush hOut b = if T.null b then return () else TIO.hPutStrLn hOut b

-- | fetchEnv
--
fetchEnv :: Either RetEnv RetEnv -> Obj.Environment
fetchEnv (Right (_, e, _)) = e
fetchEnv (Left  (_, e, _)) = e



-- | greet
--
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
