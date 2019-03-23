{-# LANGUAGE OverloadedStrings #-}
module Haskey.Lexer (lexer) where

import qualified Data.Char    as C
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
    | T.null s     = (eof, "")
    | C.isSpace ch = nextToken remain        -- skip white space
    | ch == '='    = (newToken Tk.Assign    ch, remain)
    | ch == '+'    = (newToken Tk.Plus      ch, remain)
    | ch == '-'    = (newToken Tk.Minus     ch, remain)
    | ch == '!'    = (newToken Tk.Bang      ch, remain)
    | ch == '*'    = (newToken Tk.Asterisk  ch, remain)
    | ch == '/'    = (newToken Tk.Slash     ch, remain)
    | ch == '<'    = (newToken Tk.Lt        ch, remain)
    | ch == '>'    = (newToken Tk.Gt        ch, remain)
    | ch == ';'    = (newToken Tk.Semicolon ch, remain)
    | ch == '('    = (newToken Tk.Lparen    ch, remain)
    | ch == ')'    = (newToken Tk.Rparen    ch, remain)
    | ch == ','    = (newToken Tk.Comma     ch, remain)
    | ch == '{'    = (newToken Tk.Lbrace    ch, remain)
    | ch == '}'    = (newToken Tk.Rbrace    ch, remain)
    | isLetter  ch = readIdentifire s
    | C.isDigit ch = readNumber s
    | otherwise    = (newToken Tk.Illegal   ch, remain)
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

-- | isLetter
--
isLetter :: Char -> Bool
isLetter ch = C.isAlpha ch || ch == '_'

-- | readIdentifire
--
readIdentifire :: T.Text -> (Tk.Token, T.Text)
readIdentifire s = (tok, remain)
  where
    ident = T.takeWhile isLetter s
    remain = T.dropWhile isLetter s
    tkType = Tk.lookupIdent ident
    tok = Tk.Token {Tk.tokenType = tkType, Tk.literal = ident}

-- | readNumber
--
readNumber :: T.Text -> (Tk.Token, T.Text)
readNumber s = (tok, remain)
  where
    num = T.takeWhile C.isDigit s
    remain = T.dropWhile C.isDigit s
    tok = Tk.Token {Tk.tokenType = Tk.Int, Tk.literal = num}
