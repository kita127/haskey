module Haskey.Repl (
  start
) where

import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Haskey.Ast
import           Haskey.Lexer
import           Haskey.Parser
import           Haskey.Token

prompt = ">> "

-- | start
--
start :: IO ()
start = do
    putStr prompt
    l <- TIO.getLine
    let prg = (parse . lexer) l
    TIO.putStrLn $ string prg
    start

