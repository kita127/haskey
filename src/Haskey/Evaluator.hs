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
    where newEnv k v e = Obj.Environment (M.insert k v (Obj.store e)) (Obj.outer e)

-- | get
--
get :: T.Text -> Evaluator Obj.Object
get key = Evaluator (\env -> case lookupIdent env key of
            (Just obj) -> Done obj env
            Nothing -> Error (Obj.Error (printf "identifier not found: %s" (T.unpack key))) env)


-- | lookupIdent
--
lookupIdent :: Obj.Environment -> T.Text -> Maybe Obj.Object
lookupIdent Obj.NothingEnv _ = Nothing
lookupIdent env key = case M.lookup key (Obj.store env) of
        (Just obj) -> Just obj
        Nothing    -> lookupIdent (Obj.outer env) key

-- | getEnv
--
getEnv :: Evaluator Obj.Environment
getEnv = Evaluator (\env -> Done env env)

-- | modifyEnv
--
modifyEnv :: Obj.Environment -> Evaluator ()
modifyEnv newEnv = Evaluator (\_ -> Done () newEnv)

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
    eval (Ast.FunctionLiteral _ params' body') = Obj.Function params' body' <$> getEnv
    eval (Ast.CallExpression _ func args) = do
        f <- eval func
        as <- mapM eval args
        applyFunction f as

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
evalProgram stmts = unwrapReturnValue <$> foldM givePriorityReturn Obj.Void stmts

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


-- | applyFunction
--
applyFunction :: Obj.Object -> [Obj.Object] -> Evaluator Obj.Object
applyFunction function args = do
    expectObj Obj.FUNCTION function
    let params = M.fromList $ zip (map Ast.expValue . Obj.parameters $ function) args
        extendEnv = Obj.newEnclosedEnvironment params (Obj.env function)
    evaluated <- evalInExtendEnv (Obj.body function) extendEnv
    return $ unwrapReturnValue evaluated


-- | evalInExtendEnv
--
evalInExtendEnv :: Node a => a -> Obj.Environment -> Evaluator Obj.Object
evalInExtendEnv node extendEnv = do
    env <- getEnv
    modifyEnv extendEnv
    res <- eval node
    modifyEnv env
    return res


-- | expectObj
--
expectObj :: Obj.ObjectType -> Obj.Object -> Evaluator ()
expectObj expected obj = do
    let objType = Obj.getObjectType obj
    if objType == expected
    then return ()
    else fail $ printf "not a function: %s" (show objType)

-- | unwrapReturnValue
--
unwrapReturnValue :: Obj.Object -> Obj.Object
unwrapReturnValue (Obj.ReturnValue v) = v
unwrapReturnValue obj                 = obj

-- | isTruthy
--
isTruthy :: Obj.Object -> Bool
isTruthy Obj.Null            = False
isTruthy (Obj.Boolean True ) = True
isTruthy (Obj.Boolean False) = False
isTruthy _                   = True

