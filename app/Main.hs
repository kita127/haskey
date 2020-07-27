module Main where

import           Haskey.Executor                ( repl
                                                , exec
                                                )
import           Options.Applicative
import           System.IO                      ( stdin
                                                , stdout
                                                , stderr
                                                , readFile
                                                )
import qualified Data.Text                     as T



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
        then repl stdin stdout stderr
        else do
            s <- fmap T.pack <$> readFile $ head $ args options
            exec s stdout stderr
