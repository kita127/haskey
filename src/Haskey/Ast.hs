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
, extractFailers
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
               | BlockStatement {
                   stmtToken      :: Tk.Token
                 , stmtStatements :: [Statement]
                 }
               | FailStatement {
                   stmtToken :: Tk.Token
                 , reason    :: String
                 }
               | NilStatement           -- else がない場合の値
               deriving (Eq, Show)

instance Stringer Statement where
    string (LetStatement t n v)      = Tk.literal t <> " " <> string n <> " = " <> string v <> ";"
    string (ReturnStatement t v)     = Tk.literal t <> " " <> string v <> ";"
    string (ExpressionStatement _ e) = string e
    string (BlockStatement _ ss) = T.concat . map string $ ss
    string (FailStatement _ r) = T.pack r
    string NilStatement = ""

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
                | PrefixExpression {
                    expToken :: Tk.Token
                  , operator :: T.Text
                  , right    :: Expression
                  }
                | InfixExpression {
                    expToken :: Tk.Token
                  , left     :: Expression
                  , operator :: T.Text
                  , right    :: Expression
                  }
                | Boolean {
                    expToken  :: Tk.Token
                  , boolValue :: Bool
                  }
                | IfExpression {
                    expToken    :: Tk.Token
                  , condition   :: Expression
                  , consequence :: Statement    -- BlockStatement
                  , alternative :: Statement    -- BlockStatement
                  }
                | FunctionLiteral {
                    expToken   :: Tk.Token
                  , parameters :: [Expression]    -- Identifire
                  , body       :: Statement       -- BlockStatement
                  }
                | CallExpression {
                    expToken  :: Tk.Token
                  , function  :: Expression    -- Identifire or FunctionLiteral
                  , arguments :: [Expression]
                  }
                deriving (Eq, Show)

instance Stringer Expression where
    string Nil                       = "null"
    string (Identifire _ v)          = v
    string (IntegerLiteral t _)      = Tk.literal t
    string (PrefixExpression _ o r)  = "(" <> o <> string r <> ")"
    string (InfixExpression _ l o r) = "(" <> string l <> " " <> o <> " " <> string r <> ")"
    string (Boolean t _) = Tk.literal t
    string (IfExpression _ cond cons alt) = "if " <> string cond <> " " <> string cons <> elseStr alt
    string (FunctionLiteral _ ps b) = "fn" <> "(" <> T.intercalate ", " (map string ps) <> ")" <> string b
    string (CallExpression _ f as) = string f <> "(" <> T.intercalate ", " (map string as) <> ")"


-- | elseStr
--
elseStr :: Statement -> T.Text
elseStr b@(BlockStatement _ _) = "else " <> string b
elseStr stmt                   = string stmt



-- | extractFailers
extractFailers :: Program -> [Statement]
extractFailers = filter isErrStmt . statements
  where
    isErrStmt (FailStatement _ _) = True
    isErrStmt _                   = False

-- | progra
--
program :: [Statement] -> Program
program = Program


