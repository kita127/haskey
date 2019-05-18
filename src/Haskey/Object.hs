{-# LANGUAGE OverloadedStrings #-}
module Haskey.Object
    ( Object(..)
    , ObjectType(..)
    , Environment(..)
    , inspect
    , getObjectType
    , newEnvironment
    , newEnclosedEnvironment
    )
where

import qualified Data.Map   as M
import qualified Data.Text  as T
import qualified Haskey.Ast as Ast

data ObjectType = NULL_OBJ
                | INTEGER
                | BOOLEAN
                | RETURN_VALUE_OBJ
                | FUNCTION
                | VOID
                | ERROR
    deriving (Eq, Show)


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
            | Function {
                parameters :: [Ast.Expression]      -- Identifire
              , body       :: Ast.Statement         -- BlockStatement
              , env        :: Environment
              }
            | Void      -- 返り値を返さないオブジェクト let 文など
            | Error {
                message :: String
              }
            deriving (Eq, Show)

-- TODO:
-- Evalutor 側で定義して良い気がする
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
newEnclosedEnvironment :: M.Map T.Text Object -> Environment -> Environment
newEnclosedEnvironment = Environment


-- | inspect
--
inspect :: Object -> T.Text
inspect Null              = "null"
inspect (Integer     v  ) = T.pack . show $ v
inspect (Boolean     v  ) = T.pack . show $ v
inspect (ReturnValue v  ) = inspect v
inspect (Function p b _)  = inspectFunction p b
  where
    inspectFunction param' body'
        = "fn" <>  "(" <> params param' <> ") {\n" <> Ast.string body' <> "\n}"
    params = T.intercalate ", " . map Ast.string

inspect Void              = ""
inspect (Error       msg) = T.pack $ "ERROR: " ++ msg


-- | getObjectType
--
getObjectType :: Object -> ObjectType
getObjectType Null            = NULL_OBJ
getObjectType (Integer     _) = INTEGER
getObjectType (Boolean     _) = BOOLEAN
getObjectType (ReturnValue _) = RETURN_VALUE_OBJ
getObjectType Function {}     = FUNCTION
getObjectType Void            = VOID
getObjectType (Error       _) = ERROR
