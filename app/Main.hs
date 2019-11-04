module Main where

import           Haskey.Repl
import           Haskey.Executor
import           Options.Applicative
import           System.IO                      ( stdin
                                                , stdout
                                                , stderr
                                                )



-----------------------------------------------------------------------
-- コマンドラインオプション
-----------------------------------------------------------------------
newtype Option = Option
    { args :: [String]
    } deriving (Eq, Show)

description :: String
description =
    "Haskey is a programming language written in Haskell. If you don't enter any commands, repl runs."

myOpt :: Parser Option
myOpt = Option <$> argOption
--  <*> hogeOption



  where
    argOption :: Parser [String]
    argOption =
        many $ strArgument $ help "input files" <> metavar "FILE" <> action
            "file"        -- bash に補完をおまかせする

parserInfo :: ParserInfo Option
parserInfo =
    info (helper <*> myOpt) $ fullDesc <> progDesc description <> header
        "Haskey programming language"

-----------------------------------------------------------------------

-- | main
--
-- TODO:
-- ユーザー名といらっしゃいの挨拶文の出力がまだ未対応
--
main :: IO ()
main = do
    options <- execParser parserInfo
    if null (args options)
        then start stdin stdout stdout
        else execute $ head $ args options
