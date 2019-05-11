{-# LANGUAGE OverloadedStrings #-}
module Haskey.Evaluator
    ( eval
    )
where

import qualified Data.Text                     as T
import qualified Haskey.Ast                    as Ast
import qualified Haskey.Object                 as Obj
import           Text.Printf
import           Control.Monad                  ( foldM )

-- | null'
null' :: Obj.Object
null' = Obj.Null

type ErrorObj = Obj.Object

-- | class Node
--
class Node a where
    eval :: a -> Either ErrorObj Obj.Object

instance Node Ast.Program where
    eval = evalProgram . Ast.statements

instance Node Ast.Statement where
    eval (Ast.ExpressionStatement _ e    ) = eval e
    eval (Ast.BlockStatement      _ stmts) = evalBlockStatement stmts
    eval (Ast.ReturnStatement     _ e    ) = Obj.ReturnValue <$> eval e

instance Node Ast.Expression where
    eval (Ast.IntegerLiteral _ v) = pure $ Obj.Integer v
    eval (Ast.Boolean _ v) =
        pure (if v then Obj.Boolean True else Obj.Boolean False)
    eval (Ast.PrefixExpression _ op r ) = eval r >>= evalPrefixExpression op
    eval (Ast.InfixExpression _ l op r) = do
        l' <- eval l
        r' <- eval r
        evalInfixExpression op l' r'
    eval (Ast.IfExpression _ cond cons alte) = do
        condExpr <- eval cond
        case () of
            _ | isTruthy condExpr        -> eval cons
              | alte /= Ast.NilStatement -> eval alte
              | otherwise                -> return null'





-- | givePriorityReturn
--
givePriorityReturn :: Obj.Object -> Ast.Statement -> Either ErrorObj Obj.Object
givePriorityReturn a s =
    if Obj.getObjectType a == Obj.RETURN_VALUE_OBJ then pure a else eval s

-- | isRetOrErr
--
isRetOrErr :: Obj.Object -> Bool
isRetOrErr o =
    Obj.getObjectType o
        == Obj.RETURN_VALUE_OBJ
        || Obj.getObjectType o
        == Obj.ERROR

-- | evalProgram
--
-- TODO:
-- とりあえず文がひとつもない時は NULL を返す
--
evalProgram :: [Ast.Statement] -> Either ErrorObj Obj.Object
evalProgram stmts = foldM givePriorityReturn null' stmts >>= unwrap
  where
    unwrap o = pure
        (if Obj.getObjectType o == Obj.RETURN_VALUE_OBJ
            then Obj.returnVal o
            else o
        )

-- | evalBlockStatement
--
-- TODO:
-- とりあえず文がひとつもない時は NULL を返す
--
evalBlockStatement :: [Ast.Statement] -> Either ErrorObj Obj.Object
evalBlockStatement = foldM givePriorityReturn null'

-- | evalPrefixExpression
--
evalPrefixExpression :: T.Text -> Obj.Object -> Either ErrorObj Obj.Object
evalPrefixExpression "!" right = evalBangOperatorExpression right
evalPrefixExpression "-" right = evalMinusPrefixOperatorExpression right
evalPrefixExpression op  right = Left $ newError $ printf
    "unknown operator: %s%s"
    (T.unpack op)
    (show (Obj.getObjectType right))

-- | evalBangOperatorExpression
--
evalBangOperatorExpression :: Obj.Object -> Either ErrorObj Obj.Object
evalBangOperatorExpression (Obj.Boolean True ) = pure $ Obj.Boolean False
evalBangOperatorExpression (Obj.Boolean False) = pure $ Obj.Boolean True
evalBangOperatorExpression Obj.Null            = pure $ Obj.Boolean True
evalBangOperatorExpression _                   = pure $ Obj.Boolean False

-- | evalMinusPrefixOperatorExpression
--
evalMinusPrefixOperatorExpression :: Obj.Object -> Either ErrorObj Obj.Object
evalMinusPrefixOperatorExpression (Obj.Integer v) = pure $ Obj.Integer (-v)
evalMinusPrefixOperatorExpression o               = Left $ newError $ printf
    "unknown operator: -%s"
    (show (Obj.getObjectType o))

-- | evalInfixExpression
--
evalInfixExpression
    :: T.Text -> Obj.Object -> Obj.Object -> Either ErrorObj Obj.Object
evalInfixExpression op l r
    | objTypeL == Obj.INTEGER && objTypeR == Obj.INTEGER
    = evalIntegerInfixExpression op l r
    | objTypeL /= objTypeR
    = Left $ newError $ printf "type mismatch: %s %s %s"
                               (show objTypeL)
                               (T.unpack op)
                               (show objTypeR)
    | op == "=="
    = pure $ Obj.Boolean $ l == r
    | op == "!="
    = pure $ Obj.Boolean $ l /= r
    | otherwise
    = Left $ newError $ printf "unknown operator: %s %s %s"
                               (show objTypeL)
                               (T.unpack op)
                               (show objTypeR)
  where
    objTypeL = Obj.getObjectType l
    objTypeR = Obj.getObjectType r


-- | evalIntegerInfixExpression
--
evalIntegerInfixExpression
    :: T.Text -> Obj.Object -> Obj.Object -> Either ErrorObj Obj.Object
evalIntegerInfixExpression "+" l r =
    pure $ Obj.Integer $ Obj.intVal l + Obj.intVal r
evalIntegerInfixExpression "-" l r =
    pure $ Obj.Integer $ Obj.intVal l - Obj.intVal r
evalIntegerInfixExpression "*" l r =
    pure $ Obj.Integer $ Obj.intVal l * Obj.intVal r
evalIntegerInfixExpression "/" l r =
    pure $ Obj.Integer $ Obj.intVal l `div` Obj.intVal r
evalIntegerInfixExpression "<" l r =
    pure $ Obj.Boolean $ Obj.intVal l < Obj.intVal r
evalIntegerInfixExpression ">" l r =
    pure $ Obj.Boolean $ Obj.intVal l > Obj.intVal r
evalIntegerInfixExpression "==" l r = pure $ Obj.Boolean $ l == r
evalIntegerInfixExpression "!=" l r = pure $ Obj.Boolean $ l /= r
evalIntegerInfixExpression op   l r = Left $ newError $ printf
    "unknown operator: %s %s %s"
    (show objTypeL)
    (T.unpack op)
    (show objTypeR)
  where
    objTypeL = Obj.getObjectType l
    objTypeR = Obj.getObjectType r

-- | isTruthy
--
isTruthy :: Obj.Object -> Bool
isTruthy Obj.Null            = False
isTruthy (Obj.Boolean True ) = True
isTruthy (Obj.Boolean False) = False
isTruthy _                   = True

-- | newError
--
newError :: String -> Obj.Object
newError = Obj.Error
