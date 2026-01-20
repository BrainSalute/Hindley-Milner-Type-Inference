module Unify where

import Type
import Subst
import Data.List (nub)

-- | Unification
unify :: Type -> Type -> Either String Subst
unify (TFun l1 r1) (TFun l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  return (s2 `compose` s1)
unify (TVar u) t = varBind u t
unify t (TVar u) = varBind u t
unify t1 t2
  | t1 == t2  = return Map.empty
  | otherwise = Left $ "Cannot unify types: " ++ show t1 ++ " vs " ++ show t2

-- | Bind a type variable
varBind :: String -> Type -> Either String Subst
varBind u t
  | t == TVar u       = return Map.empty
  | u `elem` ftv t    = Left $ "Occurs check failed: " ++ u ++ " in " ++ show t
  | otherwise         = return (Map.singleton u t)
