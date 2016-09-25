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

longExprL, longExprR :: Expr
longExprL = foldl1 (:+:) (map Numb [1..1000])
longExprR = foldr1 (:+:) (map Numb [1..1000])

main :: IO ()
main = defaultMain
  [ bgroup "bench"  [ bench "left"  $ whnf (length . show) longExprL
                    , bench "right" $ whnf (length . show) longExprR ] ]
