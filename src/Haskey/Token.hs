module Haskey.Token (
  Token(..)
, TokenType(..)
) where

import qualified Data.Text as T

data Token = Token
             { tokenType :: TokenType
             , literal   :: T.Text
             }
             deriving (Eq, Show)

data TokenType = Illegal
               | Eof

               -- 識別子 + リテラル
               | Ident          -- add, foobar, x, y
               | Int            -- 1343456

               -- 演算子
               | Assign         -- "="
               | Plus           -- "+"

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
               deriving (Eq, Show)
