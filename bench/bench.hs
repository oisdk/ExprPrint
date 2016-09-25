{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Criterion.Main
import           Data.Monoid
import           Data.String
import           Data.Text.ExprPrint

data Expr = Numb Integer
          | Expr :+: Expr
          | Expr :*: Expr

newtype DiffStr = DiffStr ShowS

instance Show DiffStr where
  show (DiffStr d) = d ""

instance IsString DiffStr where
  fromString = DiffStr . showString

instance Monoid DiffStr where
  mempty = DiffStr id
  DiffStr x `mappend` DiffStr y = DiffStr (x . y)

instance Show Expr where
  show = show . showExpr (\s -> "(" <> s <> ")") (\case
    Numb n  -> Lit (DiffStr (shows n))
    x :+: y -> Binary (Operator L 3 " + ") x y
    x :*: y -> Binary (Operator L 4 " * ") x y)

eval :: Expr -> Integer
eval (Numb n) = n
eval (x :+: y) = eval x + eval y
eval (x :*: y) = eval x * eval y

longExprL, longExprR :: Expr
longExprL = let e = foldl1 (:+:) (map Numb [1..100000]) in eval e `seq` e
longExprR = let e = foldr1 (:+:) (map Numb [1..100000]) in eval e `seq` e

main :: IO ()
main = defaultMain
    [ bgroup "bench"  [ bench "left"  $ whnf (length . show) longExprL
                      , bench "right" $ whnf (length . show) longExprR ] ]
