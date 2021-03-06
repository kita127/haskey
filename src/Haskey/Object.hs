{-# LANGUAGE OverloadedStrings #-}
module Haskey.Object
    ( Object(..)
    , ObjectType(..)
    , Environment(..)
    , inspect
    , getObjectType
    , newEnvironment
    , BuiltinFunction(..)
    , newEnclosedEnvironment
    )
where

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Haskey.Ast                    as Ast

data ObjectType = NULL_OBJ
                | INTEGER
                | BOOLEAN
                | RETURN_VALUE_OBJ
                | STRING_OBJ
                | FUNCTION
                | BUILTIN_OBJ
                | ARRAY_OBJ
                | VOID
                | IO_OBJ
                | ERROR
    deriving (Eq, Show)

newtype BuiltinFunction = BuiltinFunction {runBFunc :: [Object] -> Object}

instance Eq BuiltinFunction where
    _ == _ = False

instance Show BuiltinFunction where
    show _ = "builtin function"


data Object = Null
            | Integer {
                intVal :: Integer
              }
            | Boolean {
                boolVal :: Bool
              }
            | ReturnValue {
                returnVal :: Object
              }
            | String {
                strVal :: T.Text
              }
            | Function {
                parameters :: [Ast.Expression]      -- Identifire
              , body       :: Ast.Statement         -- BlockStatement
              , env        :: Environment
              }
            | Builtin {
                fn :: BuiltinFunction
              }
            | Array {
                elements :: [Object]
              }
            | Void                      -- 返り値を返さないオブジェクト let 文など
            | IO {                      -- IO に影響あるオブジェクト
                out :: T.Text           -- 出力結果
              , result :: Object        -- オブジェクトとしての結果
              }
            | Error {
                message :: String
              }
            deriving (Eq, Show)

-- TODO:
-- Evaluator 側で定義して良い気がする
--
data Environment = Environment {
                     store :: M.Map T.Text Object
                   , outer :: Environment
                   }
                 | NothingEnv
    deriving (Eq, Show)

-- | newEnvironment
--
newEnvironment :: Environment
newEnvironment = Environment (M.fromList []) NothingEnv


-- | newEnclosedEnvironment
--
newEnclosedEnvironment :: Environment -> Environment -> Environment
newEnclosedEnvironment e@(Environment _ o) outerEnv =
    e { outer = newEnclosedEnvironment o outerEnv }
newEnclosedEnvironment NothingEnv outerEnv = outerEnv

-- | inspect
--
inspect :: Object -> T.Text
inspect Null             = "null"
inspect (Integer     v)  = T.pack . show $ v
inspect (Boolean     v)  = T.pack . show $ v
inspect (ReturnValue v)  = inspect v
inspect (String      v)  = v
inspect Builtin{}        = "builtin function"
inspect (Array es      ) = "[" <> T.intercalate ", " (map inspect es) <> "]"
inspect (Function p b _) = inspectFunction p b
  where
    inspectFunction param' body' =
        "fn" <> "(" <> params param' <> ") {\n" <> Ast.string body' <> "\n}"
    params = T.intercalate ", " . map Ast.string

inspect Void        = ""
inspect IO{}        = "IO"
inspect (Error msg) = T.pack $ "ERROR: " ++ msg


-- | getObjectType
--
getObjectType :: Object -> ObjectType
getObjectType Null            = NULL_OBJ
getObjectType (Integer     _) = INTEGER
getObjectType (Boolean     _) = BOOLEAN
getObjectType (ReturnValue _) = RETURN_VALUE_OBJ
getObjectType String{}        = STRING_OBJ
getObjectType Function{}      = FUNCTION
getObjectType Builtin{}       = BUILTIN_OBJ
getObjectType Array{}         = ARRAY_OBJ
getObjectType Void            = VOID
getObjectType IO{}            = IO_OBJ
getObjectType (Error _)       = ERROR
