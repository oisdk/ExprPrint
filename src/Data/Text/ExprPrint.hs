{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module Data.Text.ExprPrint
  ( Side(..)
  , ShowExpr(..)
  , Operator(..)
  , showExpr
  ) where

import           Control.Arrow
import           Data.Monoid

data Side = L | R deriving Eq

-- | This datatype represents a level of an expression tree, and it
-- contains all the information needed to properly print that given
-- expression.
data ShowExpr t e
  -- | An expression with no children. The argument is the expression's textual representation.
  = Lit t
  -- | An expression with one child.
  | Unary  (Operator t) e
  -- | An expression with two children.
  | Binary (Operator t) e e
  deriving Functor

-- | This datatype represents the necessary information for pretty-printing an operator
data Operator t = Operator
  { -- | The associativity of an operator. Most are left-associative. Exponentiation is one of the exceptions.
    _associativity  :: Side
    -- | Precedence is assumed to be unique.
  , _precedence     :: Int
    -- | The textual representation of the operator.
  , _representation :: t }

-- | A function for properly printing an expression tree
-- with minimal parentheses.
--
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
--     proj (x :+: y) = Binary (Operator L 3 " + ") x y
--     proj (x :*: y) = Binary (Operator L 4 " * ") x y
--     proj (x :^: y) = Binary (Operator R 5 " ^ ") x y
-- :}
--
-- >>> (1 + 2 + 3) :: Expr
-- 1 + 2 + 3
--
-- >>> (1 * 2 * 3) :: Expr
-- 1 * 2 * 3
--
-- >>> (1 * 2) + 3 :: Expr
-- 1 * 2 + 3
--
-- >>> 1 * (2 + 3) :: Expr
-- 1 * (2 + 3)
--
-- >>> (1 :^: 2) :^: 3
-- (1 ^ 2) ^ 3
--
-- >>> 1 :^: 2 :^: 3
-- 1 ^ 2 ^ 3

showExpr :: Monoid t
         => (t -> t) -- ^ This argument should be a function which parenthesizes its input.
         -> (e -> ShowExpr t e)
         -> e -> t
showExpr prns proj = rec . proj where
  rec = showAlg . fmap ((prec &&& rec) . proj)
  showAlg = \case
    Lit t                               -> t
    Unary  (Operator s r t) (p,x)       -> t <> ifPrns R s r p x
    Binary (Operator s r t) (p,x) (q,y) -> ifPrns L s r p x <> t <> ifPrns R s r q y
  ifPrns sid oa op (Just (ia,ip))
    | ip < op || ip == op && (ia /= oa || oa /= sid) = prns
  ifPrns _ _ _ _ = id
  prec = \case
    Lit _                       -> Nothing
    Unary  (Operator s r _) _   -> Just (s,r)
    Binary (Operator s r _) _ _ -> Just (s,r)
{-# INLINABLE showExpr #-}
