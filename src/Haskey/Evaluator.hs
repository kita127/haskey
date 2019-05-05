{-# LANGUAGE OverloadedStrings #-}
module Haskey.Evaluator
    ( eval
    )
where

import qualified Data.Text                     as T
import qualified Haskey.Ast                    as Ast
import qualified Haskey.Object                 as Obj

-- | null'
null' = Obj.Null

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
    eval (Ast.Boolean _ v) = if v then Obj.Boolean True else Obj.Boolean False
    eval (Ast.PrefixExpression _ op r) = evalPrefixExpression op $ eval r
    eval (Ast.InfixExpression _ l op r) =
        evalInfixExpression op (eval l) (eval r)
    eval _ = null'


-- | evalPrefixExpression
--
evalPrefixExpression :: T.Text -> Obj.Object -> Obj.Object
evalPrefixExpression "!" right = evalBangOperatorExpression right
evalPrefixExpression "-" right = evalMinusPrefixOperatorExpression right
evalPrefixExpression _   _     = null'

-- | evalBangOperatorExpression
--
evalBangOperatorExpression :: Obj.Object -> Obj.Object
evalBangOperatorExpression (Obj.Boolean True ) = Obj.Boolean False
evalBangOperatorExpression (Obj.Boolean False) = Obj.Boolean True
evalBangOperatorExpression (Obj.Null         ) = Obj.Boolean True
evalBangOperatorExpression _                   = Obj.Boolean False

-- | evalMinusPrefixOperatorExpression
--
evalMinusPrefixOperatorExpression :: Obj.Object -> Obj.Object
evalMinusPrefixOperatorExpression (Obj.Integer v) = Obj.Integer (-v)
evalMinusPrefixOperatorExpression _               = null'

-- | evalInfixExpression
--
evalInfixExpression :: T.Text -> Obj.Object -> Obj.Object -> Obj.Object
evalInfixExpression op l r
    | Obj.getObjectType l == Obj.INTEGER && Obj.getObjectType r == Obj.INTEGER
    = evalIntegerInfixExpression op l r
    | otherwise
    = null'


-- | evalIntegerInfixExpression
--
evalIntegerInfixExpression :: T.Text -> Obj.Object -> Obj.Object -> Obj.Object
evalIntegerInfixExpression "+" l r = Obj.Integer $ Obj.intVal l + Obj.intVal r
evalIntegerInfixExpression "-" l r = Obj.Integer $ Obj.intVal l - Obj.intVal r
evalIntegerInfixExpression "*" l r = Obj.Integer $ Obj.intVal l * Obj.intVal r
evalIntegerInfixExpression "/" l r =
    Obj.Integer $ Obj.intVal l `div` Obj.intVal r
evalIntegerInfixExpression _ _ _ = null'
