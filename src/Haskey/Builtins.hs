{-# LANGUAGE OverloadedStrings #-}
module Haskey.Builtins
    ( builtins
    )
where

import qualified Haskey.Object                 as Obj
import qualified Data.Text                     as T
import qualified Data.Map                      as M
import           Text.Printf

-- | builtins
--
builtins :: M.Map T.Text Obj.Object
builtins = M.fromList [("len", Obj.Builtin bLen)]

-- | bLen
--
bLen :: Obj.BuiltinFunction
bLen = Obj.BuiltinFunction f
  where
    f [arg] = if Obj.getObjectType arg == Obj.STRING_OBJ
        then Obj.Integer $ toInteger $ T.length $ Obj.strVal arg
        else
            Obj.Error
            $ printf "argument to `len` not supported, got %s"
            $ show
            $ Obj.getObjectType arg
    f args =
        Obj.Error $ printf "wrong number of arguments. got=%d, want=1" $ length
            args
