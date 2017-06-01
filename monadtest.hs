{-# LANGUAGE DeriveTraversable, LambdaCase #-}

module Simex where
import Control.Monad

data Expr a =
    App (Expr a) (Expr a)
  | Lam (Expr (Maybe a))
  | Var a
  | Plus (Expr a) (Expr a)
  | Lit Int deriving (Functor, Foldable, Traversable, Show, Eq)

instance Applicative Expr where
  pure = Var
  (<*>) = ap

instance Monad Expr where
  App f x >>= g = App (f >>= g) (x >>= g)
  Var a >>= g = g a
  Plus x y >>= g = Plus (x >>= g) (y >>= g)
  Lit x >>= _ = Lit x
  Lam e >>= g = Lam $ e >>= \case
    Nothing -> Var Nothing
    Just x -> fmap Just (g x)

main :: IO ()
main = print "test"
