{-# LANGUAGE DeriveFunctor, LambdaCase #-}

module Data.Text.ExprPrint where

import Control.Arrow
import Data.Monoid

data Side = L | R deriving Eq

data ShowExpr t e = Lit t
                  | Unary  Side Int t e
                  | Binary Side Int e t e
                  deriving Functor

showExpr :: Monoid t
         => (t -> t)
         -> (e -> ShowExpr t e)
         -> e -> t
showExpr prns proj = rec . proj where
  rec = showAlg . fmap ((prec &&& rec) . proj)
  showAlg = \case
    Lit t -> t
    Unary s r t (p,x) -> t <> ifPrns R s r p x
    Binary s r (p,x) t (q,y) -> ifPrns L s r p x <> t <> ifPrns R s r q y
  ifPrns sid oa op (Just (ia,ip))
    | ip < op || ip == op && (ia /= oa || oa /= sid) = prns
  ifPrns _ _ _ _ = id
  prec = \case
    Lit _ -> Nothing
    Unary s r _ _ -> Just (s,r)
    Binary s r _ _ _ -> Just (s,r)
