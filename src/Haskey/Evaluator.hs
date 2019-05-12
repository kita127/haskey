{-# LANGUAGE OverloadedStrings #-}
module Haskey.Evaluator
    ( eval
    , runEvalutor
    , Result(..)
    )
where

import           Control.Monad (foldM)
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Haskey.Ast    as Ast
import qualified Haskey.Object as Obj
import           Text.Printf




-----------------------------------------------------------------------------

newtype Evaluator a = Evaluator {runEvalutor :: Obj.Environment -> Result a}

data Result a = Done a Obj.Environment
            | Error Obj.Object Obj.Environment
    deriving (Eq, Show)

instance Functor Evaluator where
   -- fmap :: (a -> b) -> f a -> f b
    fmap f evalutor = Evaluator
        (\env -> case runEvalutor evalutor env of
            (Error obj env') -> Error obj env'
            (Done a env')    -> Done (f a) env'
        )

instance Applicative Evaluator where
   -- pure :: a -> f a
    pure v = Evaluator (\env -> Done v env)
-- <*> :: f (a -> b) -> f a -> f b
    af <*> ax = Evaluator
        (\env -> case runEvalutor af env of
            (Error obj env') -> Error obj env'
            (Done a env')    -> runEvalutor (fmap a ax) env'
        )

instance Monad Evaluator where
   -- (>>=) :: m a -> (a -> m b) -> m b
    mx >>= f = Evaluator
        (\env -> case runEvalutor mx env of
            (Error obj env') -> Error obj env'
            (Done a env')    -> runEvalutor (f a) env'
        )
-- return :: a -> m a
-- return's default implementation is pure

-- fail :: String -> m a
    fail s = Evaluator (\env -> Error (Obj.Error s) env)


-- | set
--
set :: T.Text -> Obj.Object -> Evaluator Obj.Object
set key value = Evaluator (\env -> Done value (newEnv key value env))
    where newEnv k v e = Obj.Environment $ M.insert k v (Obj.store e)

-- | get
--
get :: T.Text -> Evaluator Obj.Object
get key = Evaluator
    (\env -> case M.lookup key (Obj.store env) of
        (Just obj) -> Done obj env
        Nothing ->
            Error (Obj.Error (printf "identifier not found: %s" (T.unpack key))) env
    )

-----------------------------------------------------------------------------

-- | null'
null' :: Obj.Object
null' = Obj.Null

type ErrorObj = Obj.Object

-- | class Node
--
class Node a where
    eval :: a -> Evaluator Obj.Object

instance Node Ast.Program where
    eval = evalProgram . Ast.statements

instance Node Ast.Statement where
    eval (Ast.ExpressionStatement _ e    ) = eval e
    eval (Ast.BlockStatement      _ stmts) = evalBlockStatement stmts
    eval (Ast.ReturnStatement     _ e    ) = Obj.ReturnValue <$> eval e
    eval (Ast.LetStatement _ name v      ) = eval v >>= set (Ast.expValue name) >> return Obj.Void

instance Node Ast.Expression where
    eval (Ast.Identifire     _ v) = evalIdentifire v
    eval (Ast.IntegerLiteral _ v) = pure $ Obj.Integer v
    eval (Ast.Boolean _ v) = pure (if v then Obj.Boolean True else Obj.Boolean False)
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
givePriorityReturn :: Obj.Object -> Ast.Statement -> Evaluator Obj.Object
givePriorityReturn a s =
    if Obj.getObjectType a == Obj.RETURN_VALUE_OBJ then pure a else eval s


-- | evalProgram
--
evalProgram :: [Ast.Statement] -> Evaluator Obj.Object
evalProgram stmts = foldM givePriorityReturn Obj.Void stmts >>= unwrap
  where
    unwrap o = pure
        (if Obj.getObjectType o == Obj.RETURN_VALUE_OBJ
            then Obj.returnVal o
            else o
        )

-- | evalBlockStatement
--
evalBlockStatement :: [Ast.Statement] -> Evaluator Obj.Object
evalBlockStatement = foldM givePriorityReturn Obj.Void

-- | evalIdentifire
--
evalIdentifire :: T.Text -> Evaluator Obj.Object
evalIdentifire name = get name

-- | evalPrefixExpression
--
evalPrefixExpression :: T.Text -> Obj.Object -> Evaluator Obj.Object
evalPrefixExpression "!" right = evalBangOperatorExpression right
evalPrefixExpression "-" right = evalMinusPrefixOperatorExpression right
evalPrefixExpression op  right = fail $ printf
    "unknown operator: %s%s"
    (T.unpack op)
    (show (Obj.getObjectType right))

-- | evalBangOperatorExpression
--
evalBangOperatorExpression :: Obj.Object -> Evaluator Obj.Object
evalBangOperatorExpression (Obj.Boolean True ) = pure $ Obj.Boolean False
evalBangOperatorExpression (Obj.Boolean False) = pure $ Obj.Boolean True
evalBangOperatorExpression Obj.Null            = pure $ Obj.Boolean True
evalBangOperatorExpression _                   = pure $ Obj.Boolean False

-- | evalMinusPrefixOperatorExpression
--
evalMinusPrefixOperatorExpression :: Obj.Object -> Evaluator Obj.Object
evalMinusPrefixOperatorExpression (Obj.Integer v) = pure $ Obj.Integer (-v)
evalMinusPrefixOperatorExpression o =
    fail $ printf "unknown operator: -%s" (show (Obj.getObjectType o))

-- | evalInfixExpression
--
evalInfixExpression :: T.Text -> Obj.Object -> Obj.Object -> Evaluator Obj.Object
evalInfixExpression op l r
    | objTypeL == Obj.INTEGER && objTypeR == Obj.INTEGER = evalIntegerInfixExpression op l r
    | objTypeL /= objTypeR = fail $ printf "type mismatch: %s %s %s"
                    (show objTypeL)
                    (T.unpack op)
                    (show objTypeR)
    | op == "==" = pure $ Obj.Boolean $ l == r
    | op == "!=" = pure $ Obj.Boolean $ l /= r
    | otherwise = fail $ printf "unknown operator: %s %s %s"
                    (show objTypeL)
                    (T.unpack op)
                    (show objTypeR)
  where
    objTypeL = Obj.getObjectType l
    objTypeR = Obj.getObjectType r


-- | evalIntegerInfixExpression
--
evalIntegerInfixExpression
    :: T.Text -> Obj.Object -> Obj.Object -> Evaluator Obj.Object
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
evalIntegerInfixExpression op   l r = fail $ printf
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

