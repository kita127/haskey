module Haskey.Evaluator
(
  eval
) where

import qualified Haskey.Ast    as Ast
import qualified Haskey.Object as Obj

-- | objNull
objNull = Obj.Null

-- | class Node
--
class Node a where
    eval :: a -> Obj.Object

instance Node Ast.Program where
    eval = last . map eval . Ast.statements

instance Node Ast.Statement where
    eval (Ast.ExpressionStatement _ e) = eval e

instance Node Ast.Expression where
    eval (Ast.IntegerLiteral _ v) = Obj.Integer v
    eval (Ast.Boolean _ v)        = if v then Obj.Boolean True else Obj.Boolean False

