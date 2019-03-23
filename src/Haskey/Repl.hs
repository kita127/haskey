module Haskey.Repl () where

import qualified Data.Text    as T
import qualified Data.Text.IO as TO
import           Haskey.Lexer
import           Haskey.Token

prompt = ">> "

-- | start
--
start :: IO ()
start = do
    putStr prompt
    l <- TO.getLine
    let ts = lexer l
    mapM_ (putStrLn . show) ts
    start

