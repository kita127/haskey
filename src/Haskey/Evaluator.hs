{-# LANGUAGE OverloadedStrings #-}
module Haskey.Evaluator
    ( eval
    )
where

import qualified Data.Text                     as T
import qualified Haskey.Ast                    as Ast
import qualified Haskey.Object                 as Obj

-- | null'
null' :: Obj.Object
null' = Obj.Null

-- | class Node
--
class Node a where
    eval :: a -> Obj.Object

instance Node Ast.Program where
    eval = evalProgram . Ast.statements

instance Node Ast.Statement where
    eval (Ast.ExpressionStatement _ e    ) = eval e
    eval (Ast.BlockStatement      _ stmts) = evalBlockStatement stmts
    eval (Ast.ReturnStatement     _ e    ) = Obj.ReturnValue $ eval e

instance Node Ast.Expression where
    eval (Ast.IntegerLiteral _ v) = Obj.Integer v
    eval (Ast.Boolean _ v) = if v then Obj.Boolean True else Obj.Boolean False
    eval (Ast.PrefixExpression _ op r) = evalPrefixExpression op $ eval r
    eval (Ast.InfixExpression _ l op r) =
        evalInfixExpression op (eval l) (eval r)
    eval (Ast.IfExpression _ cond cons alte)
        | isTruthy (eval cond)     = eval cons
        | alte /= Ast.NilStatement = eval alte
        | otherwise                = null'
    eval _ = null'


-- | givePriorityReturn
--
givePriorityReturn :: Obj.Object -> Ast.Statement -> Obj.Object
givePriorityReturn a s =
    if Obj.getObjectType a == Obj.RETURN_VALUE_OBJ then a else eval s

-- | evalProgram
--
-- TODO:
-- とりあえず文がひとつもない時は NULL を返す
--
evalProgram :: [Ast.Statement] -> Obj.Object
evalProgram = unwrap . foldl givePriorityReturn null'
  where
    unwrap o = if Obj.getObjectType o == Obj.RETURN_VALUE_OBJ
        then Obj.returnVal o
        else o

-- | evalBlockStatement
--
-- TODO:
-- とりあえず文がひとつもない時は NULL を返す
--
evalBlockStatement :: [Ast.Statement] -> Obj.Object
evalBlockStatement = foldl givePriorityReturn null'

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
evalBangOperatorExpression Obj.Null            = Obj.Boolean True
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
    | op == "=="
    = Obj.Boolean $ l == r
    | op == "!="
    = Obj.Boolean $ l /= r
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
evalIntegerInfixExpression "<"  l r = Obj.Boolean $ Obj.intVal l < Obj.intVal r
evalIntegerInfixExpression ">"  l r = Obj.Boolean $ Obj.intVal l > Obj.intVal r
evalIntegerInfixExpression "==" l r = Obj.Boolean $ l == r
evalIntegerInfixExpression "!=" l r = Obj.Boolean $ l /= r
evalIntegerInfixExpression _    _ _ = null'

-- | isTruthy
--
isTruthy :: Obj.Object -> Bool
isTruthy Obj.Null            = False
isTruthy (Obj.Boolean True ) = True
isTruthy (Obj.Boolean False) = False
isTruthy _                   = True
