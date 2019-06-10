module Haskey.Executor (execute) where

import           Data.Text.IO as TIO


-- | execute
--
-- TODO:
-- まだファイルの中身をそのまま出力するだけ
--
execute :: String -> IO ()
execute path = do
    contents <- TIO.readFile path
    TIO.putStrLn contents
    return ()
