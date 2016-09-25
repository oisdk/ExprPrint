{-# LANGUAGE DeriveFunctor, LambdaCase #-}

module Data.Text.ExprPrint where

import Control.Arrow
import Data.Monoid

data Side = L | R deriving Eq

data ShowExpr t e = Lit t
                  | Unary  Side Int t e
                  | Binary Side Int e t e
                  deriving Functor

-- | A function for properly printing an expression tree
-- with minimal parentheses.
-- >>> :{
-- data Expr = Number Integer
--           | Expr :+: Expr
--           | Expr :*: Expr
--           | Expr :^: Expr
-- infixr 8 :^:
-- infixl 6 :+:
-- infixl 7 :*:
-- :}
--
-- >>> :{
-- instance Num Expr where
--   (+) = (:+:)
--   (*) = (:*:)
--   fromInteger = Number
--   abs = undefined
--   signum = undefined
--   negate = undefined
-- :}
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.String
-- >>> import Data.Monoid
-- >>> :{
-- newtype DiffStr = DiffStr { runDiffStr :: ShowS }
-- instance Monoid DiffStr where
--   mempty = DiffStr id
--   DiffStr x `mappend` DiffStr y = DiffStr (x . y)
-- instance IsString DiffStr where fromString = DiffStr . showString
-- :}
--
-- >>> :{
-- instance Show Expr where
--   showsPrec _ = runDiffStr . showExpr (\s -> "(" <> s <> ")") proj where
--     proj (Number n) = Lit (DiffStr (shows n))
--     proj (x :+: y) = Binary L 3 x " + " y
--     proj (x :*: y) = Binary L 4 x " * " y
--     proj (x :^: y) = Binary R 5 x " ^ " y
-- :}
--
-- >>> (1 + 2 + 3) :: Expr
-- 1 + 2 + 3
-- >>> (1 * 2 * 3) :: Expr
-- 1 * 2 * 3
-- >>> (1 * 2) + 3 :: Expr
-- 1 * 2 + 3
-- >>> 1 * (2 + 3) :: Expr
-- 1 * (2 + 3)
-- >>> (1 :^: 2) :^: 3
-- (1 ^ 2) ^ 3
-- >>> 1 :^: 2 :^: 3
-- 1 ^ 2 ^ 3

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

