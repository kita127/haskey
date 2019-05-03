module Haskey.Evaluator
(
  eval
) where

import qualified Haskey.Object  as Obj
import qualified Haskey.Ast  as Ast

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

