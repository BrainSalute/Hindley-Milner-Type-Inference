module Subst where

import Type
import qualified Data.Map as Map
import Data.List (nub)

type Subst = Map.Map String Type

-- | Substitutable class
class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> [String]

instance Substitutable Type where
  apply s (TVar n) = Map.findWithDefault (TVar n) n s
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)

  ftv (TVar n) = [n]
  ftv (TFun t1 t2) = nub $ ftv t1 ++ ftv t2

instance Substitutable Scheme where
  apply s (Forall vars t) = Forall vars (apply s' t)
    where s' = foldr Map.delete s vars
  ftv (Forall vars t) = ftv t \\ vars
    where (\\) = foldl (flip delete)
          delete x xs = filter (/= x) xs

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv = nub . concatMap ftv

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1
