{-# LANGUAGE DeriveDataTypeable #-}
module Syntax where

import Data.Data

type Name = String

data Expr =
    Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  deriving (Show, Typeable, Data)

-- | Smart constructor for a chain of lambda abstractions.
lam :: [Name] -> Expr -> Expr
lam xs e = foldr Lam e xs

-- | Smart constructor for a let function binding.
letfun :: Name -> [Name] -> Expr -> Expr -> Expr
letfun x xs e1 e2 = Let x (lam xs e1) e2
