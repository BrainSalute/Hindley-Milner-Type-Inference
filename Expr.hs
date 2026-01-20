module Expr where

-- | Lambda calculus expressions
data Expr
  = Var String
  | Lam String Expr
  | App Expr Expr
  deriving (Show, Eq)
