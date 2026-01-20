module Infer where

import Expr
import Type
import Subst
import Unify
import qualified Data.Map as Map

-- | Type generator monad
newtype TypeGen a = TypeGen { runTypeGen :: Int -> (a, Int) }

instance Functor TypeGen where
  fmap f m = TypeGen $ \s ->
    let (x, s') = runTypeGen m s
    in (f x, s')

instance Applicative TypeGen where
  pure x = TypeGen $ \s -> (x, s)
  mf <*> mx = TypeGen $ \s ->
    let (f, s') = runTypeGen mf s
        (x, s'') = runTypeGen mx s'
    in (f x, s'')

instance Monad TypeGen where
  return = pure
  m >>= f = TypeGen $ \s ->
    let (x, s') = runTypeGen m s
    in runTypeGen (f x) s'

fresh :: TypeGen Type
fresh = TypeGen $ \s -> (TVar ("t" ++ show s), s + 1)

-- | Instantiate a type scheme
instantiate :: Scheme -> TypeGen Type
instantiate (Forall vars t) = do
  freshVars <- mapM (const fresh) vars
  let s = Map.fromList (zip vars freshVars)
  return $ apply s t

-- | Generalize a type
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vars t
  where vars = ftv t \\ ftv env
        (\\) = foldl (flip delete)
        delete x xs = filter (/= x) xs

-- | Type inference
infer :: TypeEnv -> Expr -> TypeGen (Either String (Subst, Type))
infer env (Var x) =
  case Map.lookup x env of
    Nothing -> return $ Left $ "Unbound variable: " ++ x
    Just sigma -> do
      t <- instantiate sigma
      return $ Right (Map.empty, t)

infer env (Lam x e) = do
  tv <- fresh
  let env' = Map.insert x (Forall [] tv) env
  r <- infer env' e
  case r of
    Left err -> return $ Left err
    Right (s1, t1) -> return $ Right (s1, TFun (apply s1 tv) t1)

infer env (App e1 e2) = do
  r1 <- infer env e1
  r2 <- infer env e2
  case (r1, r2) of
    (Right (s1, t1), Right (s2, t2)) -> do
      tv <- fresh
      case unify (apply s2 t1) (TFun t2 tv) of
        Left err -> return $ Left err
        Right s3 -> return $ Right (s3 `compose` s2 `compose` s1, apply s3 tv)
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err

-- | Run type inference
runInfer :: Expr -> Either String Type
runInfer expr = 
  let (res, _) = runTypeGen (infer Map.empty expr) 0
  in case res of
       Left err -> Left err
       Right (_, t) -> Right t
