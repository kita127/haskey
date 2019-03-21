{-# LANGUAGE OverloadedStrings #-}
module Haskey.Lexer (lexer) where

import qualified Data.Text    as T
import           Haskey.Token as Tk


-- | lexer
--
lexer :: T.Text -> [Tk.Token]
lexer s
    | Tk.tokenType tok == Tk.Eof = [tok]
    | otherwise = tok : lexer s'
  where
    (tok, s') = nextToken s

-- | nextToken
--
nextToken :: T.Text -> (Tk.Token, T.Text)
nextToken s
    | T.length s == 0 = (eof, "")
    | ch == '=' = (newToken Tk.Assign    ch, remain)
    | ch == ';' = (newToken Tk.Semicolon ch, remain)
    | ch == '(' = (newToken Tk.Rparen    ch, remain)
    | ch == ')' = (newToken Tk.Lparen    ch, remain)
    | ch == ',' = (newToken Tk.Comma     ch, remain)
    | ch == '+' = (newToken Tk.Plus      ch, remain)
    | ch == '{' = (newToken Tk.Rbrace    ch, remain)
    | ch == '}' = (newToken Tk.Lbrace    ch, remain)
  where
    ch = T.head s
    remain = T.tail s
    eof = Tk.Token {Tk.tokenType = Tk.Eof, Tk.literal = ""}

-- | newToken
--
newToken :: Tk.TokenType -> Char -> Tk.Token
newToken t c = Tk.Token { Tk.tokenType = t
                        , Tk.literal = T.singleton c
                        }
