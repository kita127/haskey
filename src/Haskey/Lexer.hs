{-# LANGUAGE OverloadedStrings #-}
module Haskey.Lexer (lexer) where

import qualified Data.Text    as T
import           Haskey.Token as Tk


-- | lexer
--
lexer :: T.Text -> [Tk.Token]
lexer s
    | T.length s == 0 = []
    | otherwise       = let (tk, s') = nextToken s in tk : lexer s'

-- | nextToken
--
nextToken :: T.Text -> (Tk.Token, T.Text)
nextToken s
    | T.length s == 0 = (Tk.Token {Tk.tokenType = Tk.Eof, Tk.literal = ""}, s)
    | ch == '=' = (newToken Tk.Assign ch, T.tail s)
    | ch == ';' = (newToken Tk.Assign ch, T.tail s)
    | ch == '(' = (newToken Tk.Assign ch, T.tail s)
    | ch == ')' = (newToken Tk.Assign ch, T.tail s)
    | ch == ',' = (newToken Tk.Assign ch, T.tail s)
    | ch == '+' = (newToken Tk.Assign ch, T.tail s)
    | ch == '{' = (newToken Tk.Assign ch, T.tail s)
    | ch == '}' = (newToken Tk.Assign ch, T.tail s)
  where
    ch = T.head s

-- | newToken
--
newToken :: Tk.TokenType -> Char -> Tk.Token
newToken t c = Tk.Token { Tk.tokenType = t
                        , Tk.literal = T.singleton c
                        }
