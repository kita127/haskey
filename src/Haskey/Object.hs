{-# LANGUAGE OverloadedStrings #-}
module Haskey.Object
    ( Object(..)
    , ObjectType(..)
    , inspect
    , getObjectType
    )
where

import qualified Data.Text                     as T
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
