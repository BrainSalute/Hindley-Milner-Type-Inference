module Main where

import Expr
import Infer

-- | Example expressions
idExpr :: Expr
idExpr = Lam "x" (Var "x")

appExpr :: Expr
appExpr = App idExpr idExpr

main :: IO ()
main = do
  putStrLn "Type inference examples:"
  print $ runInfer idExpr   -- Right (t0 -> t0)
  print $ runInfer appExpr  -- Right (t1 -> t1)
