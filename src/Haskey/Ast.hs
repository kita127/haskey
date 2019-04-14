{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Haskey.Ast
(
  Program
, Statement(..)
, Expression(..)
, program
, statements
) where

import qualified Data.Text    as T
import qualified Haskey.Token as Tk

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

data Statement = LetStatement {
                   token :: Tk.Token
                 , name  :: Expression  -- Identifire
                 , value :: Expression
                 } deriving (Eq, Show)

-- TODO:
-- Nil は最終的に削除
--
-- TODO:
-- レコードの重複をなんらかの方法でできるようにする
data Expression = Nil
                | Identifire {
                    token   :: Tk.Token
                  , idValue :: T.Text
                  }
                deriving (Eq, Show)


program :: [Statement] -> Program
program stmts = Program stmts
