{-# LANGUAGE OverloadedStrings #-}
module Haskey.Object
    ( Object(..)
    , ObjectType(..)
    , Environment
    , inspect
    , getObjectType
    , newEnvironment
    )
where

import qualified Data.Text                     as T
import qualified Data.Map                      as M

data ObjectType = NULL_OBJ
                | INTEGER
                | BOOLEAN
                | RETURN_VALUE_OBJ
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
            | Error {
                message :: String
              }
            deriving (Eq, Show)

data Environment = Environment {
                     store :: M.Map T.Text Object
                   }
    deriving (Eq, Show)

-- | newEnvironment
--
newEnvironment :: Environment
newEnvironment = Environment $ M.fromList []

-- | getEnv
--
getEnv :: Environment -> T.Text -> Maybe Object
getEnv env name = M.lookup name (store env)

-- | setEnv
--
setEnv :: Environment -> T.Text -> Object -> Environment
setEnv = undefined


-- | inspect
--
inspect :: Object -> T.Text
inspect Null              = "null"
inspect (Integer     v  ) = T.pack . show $ v
inspect (Boolean     v  ) = T.pack . show $ v
inspect (ReturnValue v  ) = inspect v
inspect (Error       msg) = T.pack $ "ERROR: " ++ msg


-- | getObjectType
--
getObjectType :: Object -> ObjectType
getObjectType Null            = NULL_OBJ
getObjectType (Integer     _) = INTEGER
getObjectType (Boolean     _) = BOOLEAN
getObjectType (ReturnValue _) = RETURN_VALUE_OBJ
getObjectType (Error       _) = ERROR
