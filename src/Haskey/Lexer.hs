{-# LANGUAGE OverloadedStrings #-}
module Haskey.Lexer
    ( lexicalize
    )
where

import qualified Data.Char                     as C
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import           Haskey.Token                  as Tok


-- | lexicalize
--
lexicalize :: T.Text -> [Tok.Token]
lexicalize s | Tok.tokenType tok == Tok.Eof = [tok]
             | otherwise                    = tok : lexicalize s'
    where (tok, s') = nextToken s

-- | nextToken
--
nextToken :: T.Text -> (Tok.Token, T.Text)
nextToken s | T.null s            = (eof, "")
            | C.isSpace ch        = nextToken remain
            |        -- skip white space
              T.isPrefixOf "==" s = readFix Tok.Eq "==" s
            | T.isPrefixOf "!=" s = readFix Tok.NotEq "!=" s
            | ch == '='           = (newToken Tok.Assign ch, remain)
            | ch == '+'           = (newToken Tok.Plus ch, remain)
            | ch == '-'           = (newToken Tok.Minus ch, remain)
            | ch == '!'           = (newToken Tok.Bang ch, remain)
            | ch == '*'           = (newToken Tok.Asterisk ch, remain)
            | ch == '/'           = (newToken Tok.Slash ch, remain)
            | ch == '<'           = (newToken Tok.Lt ch, remain)
            | ch == '>'           = (newToken Tok.Gt ch, remain)
            | ch == ';'           = (newToken Tok.Semicolon ch, remain)
            | ch == '('           = (newToken Tok.Lparen ch, remain)
            | ch == ')'           = (newToken Tok.Rparen ch, remain)
            | ch == ','           = (newToken Tok.Comma ch, remain)
            | ch == '{'           = (newToken Tok.Lbrace ch, remain)
            | ch == '}'           = (newToken Tok.Rbrace ch, remain)
            | ch == '['           = (newToken Tok.Lbracket ch, remain)
            | ch == ']'           = (newToken Tok.Rbracket ch, remain)
            | ch == '"'           = readString s
            | isLetter ch         = readIdentifire s
            | C.isDigit ch        = readNumber s
            | otherwise           = (newToken Tok.Illegal ch, remain)
  where
    ch     = T.head s
    remain = T.tail s
    eof    = Tok.Token { Tok.tokenType = Tok.Eof, Tok.literal = "" }

-- | newToken
--
newToken :: Tok.TokenType -> Char -> Tok.Token
newToken t c = Tok.Token { Tok.tokenType = t, Tok.literal = T.singleton c }

-- | isLetter
--
isLetter :: Char -> Bool
isLetter ch = C.isAlpha ch || ch == '_'

-- | readIdentifire
--
readIdentifire :: T.Text -> (Tok.Token, T.Text)
readIdentifire s = (tok, remain)
  where
    ident  = T.takeWhile isLetter s
    remain = T.dropWhile isLetter s
    tkType = Tok.lookupIdent ident
    tok    = Tok.Token { Tok.tokenType = tkType, Tok.literal = ident }

-- | readNumber
--
readNumber :: T.Text -> (Tok.Token, T.Text)
readNumber s = (tok, remain)
  where
    num    = T.takeWhile C.isDigit s
    remain = T.dropWhile C.isDigit s
    tok    = Tok.Token { Tok.tokenType = Tok.Int, Tok.literal = num }

readString :: T.Text -> (Tok.Token, T.Text)
readString s = (tok, remain)
  where
    tok    = Tok.Token Tok.STRING lit
    remain = T.tail . T.dropWhile (/= '"') . T.tail $ s
    lit    = T.takeWhile (/= '"') . T.tail $ s

-- | readFix
--
readFix :: Tok.TokenType -> T.Text -> T.Text -> (Tok.Token, T.Text)
readFix tkType fix s = (tok, remain)
  where
    remain = fromJust $ T.stripPrefix fix s
    word   = T.take (T.length fix) s
    tok    = Tok.Token { Tok.tokenType = tkType, Tok.literal = word }
