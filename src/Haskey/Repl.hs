{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Haskey.Repl (
  start
) where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Haskey.Ast        as Ast
import           Haskey.Lexer
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
    let prg = (parse . lexer) l

    if hasError prg
    then do
        printParseError prg
        start
    else do
        TIO.putStrLn $ Ast.string prg
        start


-- | hasError
--
hasError :: Ast.Program -> Bool
hasError = ( /= 0) . length . Ast.extractFailers




-- | printParseError
--
printParseError :: Ast.Program -> IO ()
printParseError prg = do
    TIO.putStrLn chobiFace
    mapM_ (TIO.putStrLn . ("\t" <>) . Ast.string) . Ast.extractFailers $ prg

chobiFace :: T.Text
chobiFace = [r|
ちょび
|]
