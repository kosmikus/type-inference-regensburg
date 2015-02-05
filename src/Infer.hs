{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Control.Applicative hiding ((<|>))
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Error
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Syntax
import Types
import Repl

-- Free type variables and substitution

type Subst = Map Name MType

class Substitutable a where
  free :: a -> Set Name
  subst :: Subst -> a -> a

instance Substitutable MType where
  free (TVar n)    = S.singleton n
  free (Con _)     = S.empty
  free (List t1)   = free t1
  free (Arr t1 t2) = free t1 `S.union` free t2

  subst s (TVar n)    = M.findWithDefault (TVar n) n s
  subst _ (Con n)     = Con n
  subst s (List t1)   = List (subst s t1)
  subst s (Arr t1 t2) = Arr (subst s t1) (subst s t2)

instance Substitutable PType where
  free (Forall xs t) = free t `S.difference` S.fromList xs

  subst s (Forall xs t) = Forall xs (subst (foldr M.delete s xs) t)

instance Substitutable Env where
  free gamma = S.unions (map free (M.elems gamma))

  subst s gamma = M.map (subst s) gamma

identity :: Subst
identity = M.empty

comp :: Subst -> Subst -> Subst
comp s1 s2 = M.union s1 (M.map (subst s1) s2)

-- Actual inference

infer :: Env -> Expr -> Infer (Subst, MType)
infer gamma (Var x) = do
  sigma <- lookupEnv x gamma
  tau <- inst sigma
  return (identity, tau)

infer gamma (App e1 e2) = do
  (s1, tau1) <- infer gamma e1
  (s2, tau2) <- infer (subst s1 gamma) e2
  tau3 <- fresh
  s3 <- unify (subst s2 tau1) (Arr tau2 tau3) 
  return (s3 `comp` s2 `comp` s1, subst s3 tau3)

infer gamma (Lam x e) = do
  tau1 <- fresh
  (s, tau2) <- infer (M.insert x (Forall [] tau1) gamma) e
  return (s, Arr (subst s tau1) tau2)

infer gamma (Let x e1 e2) = do
  (s1, tau1) <- infer gamma e1
  let gamma' = subst s1 gamma
  let sigma = gen gamma' tau1
  (s2, tau2) <- infer (M.insert x sigma gamma') e2
  return (s2 `comp` s1, tau2)

unify :: MType -> MType -> Infer Subst
unify (TVar n) t = bind n t
unify t (TVar n) = bind n t
unify (Con n1) (Con n2) | n1 == n2 = return identity
unify (Arr a1 a2) (Arr b1 b2) = do
  s1 <- unify a1 b1
  s2 <- unify (subst s1 a2) (subst s1 b2)
  return (s2 `comp` s1)
unify (List a) (List b) =
  unify a b
unify t1 t2 = cannotUnify t1 t2

bind :: Name -> MType -> Infer Subst
bind n (TVar n') | n == n'     = return identity
bind n t | n `S.member` free t = occursCheckFailed n t
bind n t                       = return (M.singleton n t)

inst :: PType -> Infer MType
inst (Forall xs t) = do
  assocs <- forM xs $ \ x -> do
                               v <- fresh
                               return (x, v)
  return (subst (M.fromList assocs) t)

gen :: Env -> MType -> PType
gen gamma tau = Forall xs tau
  where xs = S.toList (free tau `S.difference` free gamma)

-- The monad

newtype Infer a = Infer (ErrorT String (State Int) a)
  deriving (Monad, MonadState Int, MonadError String)

runInfer :: Infer a -> Either String a
runInfer (Infer i) = evalState (runErrorT i) 0

fresh :: Infer MType
fresh = do
  c <- get
  put (c + 1)
  return (TVar ("t" ++ show c))

lookupEnv :: Name -> Env -> Infer PType
lookupEnv n gamma =
  case M.lookup n gamma of
    Nothing -> throwError $ "unknown identifier: " ++ n
    Just t  -> return t

cannotUnify :: MType -> MType -> Infer a
cannotUnify t1 t2 = throwError $ "cannot unify " ++ pretty t1 ++ " and " ++ pretty t2

occursCheckFailed :: Name -> MType -> Infer a
occursCheckFailed n t = throwError $ "occurs check failed for " ++ n ++ " in " ++ pretty t

typeOf :: Env -> Expr -> Either String PType
typeOf gamma e = runInfer $ do (_, t) <- infer gamma e
                               return (gen gamma t)

-- Testing

initGamma :: Env
initGamma = M.fromList
  [ ("n", Forall [] intTy)
  , ("True", Forall [] boolTy)
  , ("False", Forall [] boolTy)
  , ("caseBool", Forall ["a"] (boolTy .->. TVar "a" .->. TVar "a" .->. TVar "a"))
  , ("Nil", Forall ["a"] (List (TVar "a")))
  , ("Cons", Forall ["a"] (TVar "a" .->. List (TVar "a") .->. List (TVar "a")))
  , ("caseList", Forall ["a", "b"] (List (TVar "a") .->. TVar "b" .->.
                                     (TVar "a" .->. List (TVar "a") .->. TVar "b") .->. TVar "b"))
  , ("fix", Forall ["a"] ((TVar "a" .->. TVar "a") .->. TVar "a"))
  ]

main :: IO ()
main = repl initGamma typeOf

