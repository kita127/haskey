{-# LANGUAGE OverloadedStrings #-}
module Haskey.Evaluator
(
  eval
) where

import qualified Data.Text     as T
import qualified Haskey.Ast    as Ast
import qualified Haskey.Object as Obj

-- | objNull
objNull = Obj.Null

-- | class Node
--
class Node a where
    eval :: a -> Obj.Object

instance Node Ast.Program where
    eval = last . map eval . Ast.statements

instance Node Ast.Statement where
    eval (Ast.ExpressionStatement _ e) = eval e

instance Node Ast.Expression where
    eval (Ast.IntegerLiteral _ v) = Obj.Integer v
    eval (Ast.Boolean _ v)        = if v then Obj.Boolean True else Obj.Boolean False
    eval (Ast.PrefixExpression _ op r) = evalPrefixExpression op $ eval r


-- | evalPrefixExpression
--
evalPrefixExpression :: T.Text -> Obj.Object -> Obj.Object
evalPrefixExpression "!" right = evalBangOperatorExpression right
evalPrefixExpression _ _       = objNull

-- | evalBangOperatorExpression
--
evalBangOperatorExpression :: Obj.Object -> Obj.Object
evalBangOperatorExpression (Obj.Boolean True)  = Obj.Boolean False
evalBangOperatorExpression (Obj.Boolean False) = Obj.Boolean True
evalBangOperatorExpression (Obj.Null)          = Obj.Boolean True
evalBangOperatorExpression _                   = Obj.Boolean False
