{-# LANGUAGE OverloadedStrings #-}
module Haskey.Token
    ( Token(..)
    , TokenType(..)
    , lookupIdent
    , isToken
    )
where

import qualified Data.Text                     as T

data Token = Token
             { tokenType :: TokenType
             , literal   :: T.Text
             }
             deriving (Eq, Show)

-- TODO:
-- TRUE, FALSE だけ大文字なんかいやだ
--
data TokenType = Illegal
               | Eof

               -- 識別子 + リテラル
               | Ident          -- add, foobar, x, y
               | Int            -- 1343456

               -- 演算子
               | Assign         -- "="
               | Plus           -- "+"
               | Minus          -- "-"
               | Bang           -- "!"
               | Asterisk       -- "*"
               | Slash          -- "*"
               | Eq             -- "=="
               | NotEq          -- "!="

               | Lt             -- "<"
               | Gt             -- ">"

               -- デリミタ
               | Comma          -- ","
               | Semicolon      -- ";"

               | Lparen         -- "("
               | Rparen         -- ")"
               | Lbrace         -- "{"
               | Rbrace         -- "}"

               -- キーワード
               | Function       -- "fn"
               | Let            -- "let"
               | If             -- "if"
               | Else           -- "else"
               | TRUE           -- "true"
               | FALSE          -- "false"
               | Return         -- "return"

               | STRING         -- string
               deriving (Eq, Show, Ord)

-- | keywords
--
keywords :: [(T.Text, TokenType)]
keywords =
    [ ("fn"    , Function)
    , ("let"   , Let)
    , ("if"    , If)
    , ("else"  , Else)
    , ("true"  , TRUE)
    , ("false" , FALSE)
    , ("return", Return)
    ]

-- | lookupIdent
--
lookupIdent :: T.Text -> TokenType
lookupIdent ident | null ks   = Ident
                  | otherwise = snd $ head ks
    where ks = filter ((ident ==) . fst) keywords

-- | isToken
--
isToken :: TokenType -> Token -> Bool
isToken tt tok = tokenType tok == tt
