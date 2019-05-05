{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Haskey.Ast
    ( Program
    , Statement(..)
    , Expression(..)
    , string
    , program
    , statements
    , extractFailers
    )
where

import qualified Data.Text                     as T
import qualified Haskey.Token                  as Tok

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
                   stmtToken :: Tok.Token
                 , name      :: Expression  -- Identifire
                 , value     :: Expression
                 }
               | ReturnStatement {
                   stmtToken   :: Tok.Token
                 , returnValue :: Expression
                 }
               | ExpressionStatement {
                   stmtToken  :: Tok.Token
                 , expression :: Expression
                 }
               | BlockStatement {
                   stmtToken      :: Tok.Token
                 , stmtStatements :: [Statement]
                 }
               | FailStatement {
                   stmtToken :: Tok.Token
                 , reason    :: String
                 }
               | NilStatement           -- else がない場合の値
               deriving (Eq, Show)

instance Stringer Statement where
    string (LetStatement t n v) =
        Tok.literal t <> " " <> string n <> " = " <> string v <> ";"
    string (ReturnStatement     t v ) = Tok.literal t <> " " <> string v <> ";"
    string (ExpressionStatement _ e ) = string e
    string (BlockStatement      _ ss) = T.concat . map string $ ss
    string (FailStatement       _ r ) = T.pack r
    string NilStatement               = ""

-- TODO:
-- レコードの重複をなんらかの方法でできるようにする

-- | type Expression
--
data Expression = Identifire {
                    expToken :: Tok.Token
                  , expValue :: T.Text
                  }
                | IntegerLiteral {
                    expToken :: Tok.Token
                  , intValue :: Integer
                  }
                | PrefixExpression {
                    expToken :: Tok.Token
                  , operator :: T.Text
                  , right    :: Expression
                  }
                | InfixExpression {
                    expToken :: Tok.Token
                  , left     :: Expression
                  , operator :: T.Text
                  , right    :: Expression
                  }
                | Boolean {
                    expToken  :: Tok.Token
                  , boolValue :: Bool
                  }
                | IfExpression {
                    expToken    :: Tok.Token
                  , condition   :: Expression
                  , consequence :: Statement    -- BlockStatement
                  , alternative :: Statement    -- BlockStatement
                  }
                | FunctionLiteral {
                    expToken   :: Tok.Token
                  , parameters :: [Expression]    -- Identifire
                  , body       :: Statement       -- BlockStatement
                  }
                | CallExpression {
                    expToken  :: Tok.Token
                  , function  :: Expression    -- Identifire or FunctionLiteral
                  , arguments :: [Expression]
                  }
                deriving (Eq, Show)

instance Stringer Expression where
    string (Identifire     _ v    ) = v
    string (IntegerLiteral t _    ) = Tok.literal t
    string (PrefixExpression _ o r) = "(" <> o <> string r <> ")"
    string (InfixExpression _ l o r) =
        "(" <> string l <> " " <> o <> " " <> string r <> ")"
    string (Boolean t _) = Tok.literal t
    string (IfExpression _ cond cons alt) =
        "if " <> string cond <> " " <> string cons <> elseStr alt
    string (FunctionLiteral _ ps b) =
        "fn" <> "(" <> T.intercalate ", " (map string ps) <> ")" <> string b
    string (CallExpression _ f as) =
        string f <> "(" <> T.intercalate ", " (map string as) <> ")"


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
