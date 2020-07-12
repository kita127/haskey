{-# LANGUAGE OverloadedStrings #-}
module Haskey.Evaluator
    ( eval
    , runEvaluator
    , Result(..)
    , Buffer
    )
where

import           Control.Applicative
import           Control.Monad                  ( foldM
                                                , join
                                                , liftM2
                                                )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Haskey.Ast                    as Ast
import qualified Haskey.Object                 as Obj
import           Text.Printf
import qualified Haskey.Builtins               as Blt



-----------------------------------------------------------------------------

newtype Evaluator a = Evaluator {runEvaluator :: (Obj.Environment, Buffer) -> Result a}

-- | 出力する結果を格納するバッファ
--
type Buffer = T.Text

data Result a = Done a Obj.Environment Buffer
            | Error Obj.Object Obj.Environment
    deriving (Eq, Show)

instance Functor Evaluator where
   -- fmap :: (a -> b) -> f a -> f b
    fmap f evalutor = Evaluator
        (\input -> case runEvaluator evalutor input of
            (Error obj env') -> Error obj env'
            (Done a env' b ) -> Done (f a) env' b
        )

instance Applicative Evaluator where
   -- pure :: a -> f a
    pure v = Evaluator (uncurry (Done v))
-- <*> :: f (a -> b) -> f a -> f b
    af <*> ax = Evaluator
        (\input -> case runEvaluator af input of
            (Error obj env') -> Error obj env'
            (Done a env' b ) -> runEvaluator (fmap a ax) (env', b)
        )

instance Monad Evaluator where
   -- (>>=) :: m a -> (a -> m b) -> m b
    mx >>= f = Evaluator
        (\input -> case runEvaluator mx input of
            (Error obj env') -> Error obj env'
            (Done a env' b ) -> runEvaluator (f a) (env', b)
        )
-- return :: a -> m a
-- return's default implementation is pure

-- fail :: String -> m a
    fail s = Evaluator (\(env, _) -> Error (Obj.Error s) env)


instance Alternative Evaluator where
  -- many :: f a -> f [a]
  -- | (<|>)
    ea <|> eb = Evaluator
        (\input -> case runEvaluator ea input of
            r@Done{}    -> r
            (Error _ _) -> runEvaluator eb input
        )
    empty = Evaluator (\(env, _) -> Error (Obj.Error "empty Object") env)

-- | set
--
set :: T.Text -> Obj.Object -> Evaluator Obj.Object
set key value = Evaluator (\(env, b) -> Done value (newEnv key value env) b)
  where
    newEnv k v e = Obj.Environment (M.insert k v (Obj.store e)) (Obj.outer e)

-- | get
--
get :: T.Text -> Evaluator Obj.Object
get key = Evaluator
    (\(env, b) -> case lookupIdent env key of
        (Just obj) -> Done obj env b
        Nothing    -> Error
            (Obj.Error (printf "identifier not found: %s" (T.unpack key)))
            env
    )

-- | getBuiltins
--
getBuiltins :: T.Text -> Evaluator Obj.Object
getBuiltins key = Evaluator
    (\(env, b) -> case M.lookup key Blt.builtins of
        (Just obj) -> Done obj env b
        Nothing    -> Error
            (Obj.Error (printf "identifier not found: %s" (T.unpack key)))
            env
    )

-- | lookupIdent
--
lookupIdent :: Obj.Environment -> T.Text -> Maybe Obj.Object
lookupIdent Obj.NothingEnv _   = Nothing
lookupIdent env            key = case M.lookup key (Obj.store env) of
    (Just obj) -> Just obj
    Nothing    -> lookupIdent (Obj.outer env) key

-- | getEnv
--
getEnv :: Evaluator Obj.Environment
getEnv = Evaluator (\(env, b) -> Done env env b)

-- | modifyEnv
--
modifyEnv :: Obj.Environment -> Evaluator ()
modifyEnv newEnv = Evaluator (\(_, b) -> Done () newEnv b)

-- | appendBuffer
--
appendBuffer :: T.Text -> Evaluator ()
appendBuffer s = Evaluator (\(env, b) -> Done () env (b <> s))

-----------------------------------------------------------------------------

-- | null'
null' :: Obj.Object
null' = Obj.Null

-- | class Node
--
class Node a where
    eval :: a -> Evaluator Obj.Object

instance Node Ast.Program where
    eval = evalProgram . Ast.statements

instance Node Ast.Statement where
    eval (Ast.LetStatement _ name v) =
        eval v >>= set (Ast.expValue name) >> return Obj.Void
    eval (Ast.ReturnStatement _ e) = Obj.ReturnValue <$> eval e
    eval (Ast.ExpressionStatement _ e) = eval e
    eval (Ast.BlockStatement _ stmts) = evalBlockStatement stmts
    eval (Ast.FailStatement _ s) = return $ Obj.Error s
    eval _ = return $ Obj.Error "unknown node"

instance Node Ast.Expression where
    eval (Ast.Identifire     _ v) = evalIdentifire v
    eval (Ast.IntegerLiteral _ v) = pure $ Obj.Integer v
    eval (Ast.Boolean _ v) =
        pure (if v then Obj.Boolean True else Obj.Boolean False)
    eval (Ast.PrefixExpression _ op r) = eval r >>= evalPrefixExpression op
    eval (Ast.FunctionLiteral _ params' body') =
        Obj.Function params' body' <$> getEnv
    eval (Ast.CallExpression _ func args) =
        join $ liftM2 applyFunction (eval func) (mapM eval args)
    eval (Ast.InfixExpression _ l op r) =
        join $ liftM2 (evalInfixExpression op) (eval l) (eval r)
    eval (Ast.IfExpression _ cond cons alte) =
        eval cond
            >>= (\c -> if isTruthy c then eval cons else evalIfExists alte)
    eval (Ast.StringLiteral _ s ) = pure $ Obj.String s
    eval (Ast.ArrayLiteral  _ es) = Obj.Array <$> mapM eval es
    eval (Ast.IndexExpression _ left index) =
        join $ liftM2 evalIndexExpression (eval left) (eval index)

-- | evalIfExists
--
evalIfExists :: Ast.Statement -> Evaluator Obj.Object
evalIfExists Ast.NilStatement = return null'
evalIfExists alte             = eval alte


-- | givePriorityReturn
--
givePriorityReturn :: Obj.Object -> Ast.Statement -> Evaluator Obj.Object
givePriorityReturn r@(Obj.ReturnValue _) _ = pure r
givePriorityReturn _                     s = eval s


-- | evalProgram
--
evalProgram :: [Ast.Statement] -> Evaluator Obj.Object
evalProgram stmts =
    unwrapReturnValue <$> foldM givePriorityReturn Obj.Void stmts

-- | evalBlockStatement
--
evalBlockStatement :: [Ast.Statement] -> Evaluator Obj.Object
evalBlockStatement = foldM givePriorityReturn Obj.Void

-- | evalIdentifire
--
evalIdentifire :: T.Text -> Evaluator Obj.Object
evalIdentifire name = get name <|> getBuiltins name

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
evalInfixExpression
    :: T.Text -> Obj.Object -> Obj.Object -> Evaluator Obj.Object
evalInfixExpression op l r
    | objTypeL == Obj.INTEGER && objTypeR == Obj.INTEGER
    = evalIntegerInfixExpression op l r
    | objTypeL == Obj.STRING_OBJ && objTypeR == Obj.STRING_OBJ
    = evalStringInfixExpression op l r
    | objTypeL /= objTypeR
    = fail $ printf "type mismatch: %s %s %s"
                    (show objTypeL)
                    (T.unpack op)
                    (show objTypeR)
    | op == "=="
    = pure $ Obj.Boolean $ l == r
    | op == "!="
    = pure $ Obj.Boolean $ l /= r
    | otherwise
    = fail $ printf "unknown operator: %s %s %s"
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

-- | evalStringInfixExpression
--
--
evalStringInfixExpression
    :: T.Text -> Obj.Object -> Obj.Object -> Evaluator Obj.Object
evalStringInfixExpression "+" l r =
    return $ Obj.String (Obj.strVal l `T.append` Obj.strVal r)
evalStringInfixExpression op l r = fail $ printf
    "unknown operator: %s %s %s"
    (show objTypeL)
    (T.unpack op)
    (show objTypeR)
  where
    objTypeL = Obj.getObjectType l
    objTypeR = Obj.getObjectType r


-- | evalIndexExpression
--
evalIndexExpression :: Obj.Object -> Obj.Object -> Evaluator Obj.Object
evalIndexExpression left index
    | objTypeL /= Obj.ARRAY_OBJ || objTypeI /= Obj.INTEGER = fail
    $ printf "index operator not supported: %s" (show objTypeL)
    | idx < 0 || idx >= max' = pure null'
    | otherwise = pure $ Obj.elements left !! fromInteger idx
  where
    objTypeL = Obj.getObjectType left
    objTypeI = Obj.getObjectType index
    max'     = toInteger $ length $ Obj.elements left
    idx      = Obj.intVal index



-- | applyFunction
--
applyFunction :: Obj.Object -> [Obj.Object] -> Evaluator Obj.Object
applyFunction function args = do
    object <- userFunction function args <|> builtinFunction function args
    let objType = Obj.getObjectType object
    if objType == Obj.IO_OBJ
        then do
            let s = Obj.out object
            appendBuffer s
            return $ Obj.result object
        else return object

-- | userFunction
--
userFunction :: Obj.Object -> [Obj.Object] -> Evaluator Obj.Object
userFunction function args = do
    expectObj Obj.FUNCTION function
    currentEnv <- getEnv
    let params =
            M.fromList $ zip (map Ast.expValue . Obj.parameters $ function) args
-- 関数のパラメータと関数が持つ環境で新たな環境を作成
-- 新たな環境に現在の最新の環境を閉じ込める
        newEnv    = Obj.Environment params (Obj.env function)
        extendEnv = Obj.newEnclosedEnvironment newEnv currentEnv
    evaluated <- evalInExtendEnv (Obj.body function) extendEnv
    return $ unwrapReturnValue evaluated

-- | builtinFunction
--
builtinFunction :: Obj.Object -> [Obj.Object] -> Evaluator Obj.Object
builtinFunction function args = do
    expectObj Obj.BUILTIN_OBJ function
    let f = Obj.fn function
    return $ Obj.runBFunc f args

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
