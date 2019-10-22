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
builtins = M.fromList
    [ ("len"  , wrap bLen)
    , ("first", wrap bFirst)
    , ("last" , wrap bLast)
    , ("rest" , wrap bRest)
    ]

-- | wrapBf
--
wrap :: ([Obj.Object] -> Obj.Object) -> Obj.Object
wrap = Obj.Builtin . Obj.BuiltinFunction

-- | bLen
--
bLen :: [Obj.Object] -> Obj.Object
bLen [arg] = case Obj.getObjectType arg of
    Obj.STRING_OBJ -> Obj.Integer $ toInteger $ T.length $ Obj.strVal arg
    Obj.ARRAY_OBJ  -> Obj.Integer $ toInteger $ length $ Obj.elements arg
    _ ->
        Obj.Error
            $ printf "argument to `len` not supported, got %s"
            $ show
            $ Obj.getObjectType arg
bLen args =
    Obj.Error $ printf "wrong number of arguments. got=%d, want=1" $ length args

-- | bFirst
--
bFirst :: [Obj.Object] -> Obj.Object
bFirst [arg] = case Obj.getObjectType arg of
    Obj.ARRAY_OBJ ->
        if null (Obj.elements arg) then Obj.Null else head $ Obj.elements arg
    _ ->
        Obj.Error
            $ printf "argument to `first` must be ARRAY, got %s"
            $ show
            $ Obj.getObjectType arg
bFirst args =
    Obj.Error $ printf "wrong number of arguments. got=%d, want=1" $ length args

-- | bLast
--
bLast :: [Obj.Object] -> Obj.Object
bLast [arg] = case Obj.getObjectType arg of
    Obj.ARRAY_OBJ ->
        if null (Obj.elements arg) then Obj.Null else last $ Obj.elements arg
    _ ->
        Obj.Error
            $ printf "argument to `last` must be ARRAY, got %s"
            $ show
            $ Obj.getObjectType arg
bLast args =
    Obj.Error $ printf "wrong number of arguments. got=%d, want=1" $ length args

-- | bRest
--
bRest :: [Obj.Object] -> Obj.Object
bRest [arg] = case Obj.getObjectType arg of
    Obj.ARRAY_OBJ -> if null (Obj.elements arg)
        then Obj.Null
        else Obj.Array $ tail $ Obj.elements arg
    _ ->
        Obj.Error
            $ printf "argument to `rest` must be ARRAY, got %s"
            $ show
            $ Obj.getObjectType arg
bRest args =
    Obj.Error $ printf "wrong number of arguments. got=%d, want=1" $ length args
