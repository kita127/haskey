{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Haskey.Ast
(
  Program
, Statement(..)
, Expression(..)
, string
, program
, statements
) where

import qualified Data.Text    as T
import qualified Haskey.Token as Tk

-- | class Stringer
--
class Stringer a where
    string :: a -> T.Text

-- | type Program
--
newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

instance Stringer Program where
    string = T.concat . map string . statements

-- | type Statement
--
data Statement = LetStatement {
                   stmtToken :: Tk.Token
                 , name      :: Expression  -- Identifire
                 , value     :: Expression
                 }
               | ReturnStatement {
                   stmtToken   :: Tk.Token
                 , returnValue :: Expression
                 }
               | ExpressionStatement {
                   stmtToken  :: Tk.Token
                 , expression :: Expression
                 }
               | FailStatement {
                   stmtToken :: Tk.Token
                 , reason    :: String
                 }
               deriving (Eq, Show)

instance Stringer Statement where
    string (LetStatement t n v)      = Tk.literal t <> " " <> string n <> " = " <> string v <> ";"
    string (ReturnStatement t v)     = Tk.literal t <> " " <> string v <> ";"
    string (ExpressionStatement _ e) = string e <> ";"
    string (FailStatement _ r) = T.pack r

-- TODO:
-- Nil は最終的に削除
--
-- TODO:
-- レコードの重複をなんらかの方法でできるようにする

-- | type Expression
--
data Expression = Nil
                | Identifire {
                    expToken :: Tk.Token
                  , expValue :: T.Text
                  }
                | IntegerLiteral {
                    expToken :: Tk.Token
                  , intValue :: Integer
                  }
                deriving (Eq, Show)

instance Stringer Expression where
    string Nil                  = "null"
    string (Identifire _ v)     = v
    string (IntegerLiteral t _) = Tk.literal t


-- | progra
--
program :: [Statement] -> Program
program = Program


