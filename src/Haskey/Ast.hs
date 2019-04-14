{-# LANGUAGE DuplicateRecordFields #-}
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
                   stmtToken :: Tk.Token
                 , name      :: Expression  -- Identifire
                 , value     :: Expression
                 }
               | ReturnStatement {
                   stmtToken   :: Tk.Token
                 , returnValue :: Expression
                 }
               deriving (Eq, Show)

-- TODO:
-- Nil は最終的に削除
--
-- TODO:
-- レコードの重複をなんらかの方法でできるようにする
data Expression = Nil
                | Identifire {
                    expToken :: Tk.Token
                  , idValue  :: T.Text
                  }
                deriving (Eq, Show)


program :: [Statement] -> Program
program = Program
