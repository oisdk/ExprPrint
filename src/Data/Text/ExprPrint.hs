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
  -- | A prefix expression with one child.
  | Prefix  (Operator t) e
  -- | A postfix expression with one child.
  | Postfix (Operator t) e
  -- | An expression with two children.
  | Binary (Operator t) e e
  deriving  Functor

-- | This datatype represents the necessary information for pretty-printing an operator
data Operator t = Operator
  { -- | The associativity of an operator. Most are left-associative. Exponentiation is one of the exceptions.
    _associativity  :: Side
    -- | Precedence is assumed to be unique.
  , _precedence     :: Int
    -- | The textual representation of the operator.
  , _representation :: t }

-- | A function for properly printing an expression tree
-- with minimal parentheses. Algorithm is adapted from
-- <http://www.cs.tufts.edu/%7Enr/pubs/unparse-abstract.html here>.
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
-- >>> import Data.Monoid
--
-- >>> :{
-- instance Show Expr where
--   showsPrec _ = appEndo . showExpr (\s -> toEnd "(" <> s <> toEnd ")") proj where
--     proj (Number n) = Lit (Endo (shows n))
--     proj (x :+: y) = Binary (Operator L 3 (toEnd " + ")) x y
--     proj (x :*: y) = Binary (Operator L 4 (toEnd " * ")) x y
--     proj (x :^: y) = Binary (Operator R 5 (toEnd " ^ ")) x y
--     toEnd = Endo . showString
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
--
-- >>> data BoolExpr = T | F | And BoolExpr BoolExpr | Or BoolExpr BoolExpr | Not BoolExpr
--
-- >>> :{
-- instance Show BoolExpr where
--   show = showExpr (\s -> "(" ++ s ++ ")") proj where
--     proj T = Lit "1"
--     proj F = Lit "0"
--     proj (And x y) = Binary (Operator L 3 " && ") x y
--     proj (Or  x y) = Binary (Operator L 2 " || ") x y
--     proj (Not x)   = Prefix (Operator L 4 "!"   ) x
-- :}
--
-- >>> Not T
-- !1
--
-- >>> And T T
-- 1 && 1
--
-- >>> And T (Or F F)
-- 1 && (0 || 0)
--
-- >>> Or T (And F F)
-- 1 || 0 && 0
--
-- >>> Not (Or T F)
-- !(1 || 0)
--
-- >>> Not (Not F)
-- !(!0)

showExpr :: Monoid t
         => (t -> t) -- ^ This argument should be a function which parenthesizes its input.
         -> (e -> ShowExpr t e)
         -> e -> t
showExpr prns proj = rec . proj where
  rec = showAlg . fmap ((prec &&& rec) . proj)
  showAlg = \case
    Lit t                                ->                     t
    Prefix  (Operator s r t)       (q,y) ->                     t <> ifPrns R s r q y
    Postfix (Operator s r t) (p,x)       -> ifPrns L s r p x <> t
    Binary  (Operator s r t) (p,x) (q,y) -> ifPrns L s r p x <> t <> ifPrns R s r q y
  ifPrns sid oa op (Just (ia,ip))
    | ip < op || ip == op && (ia /= oa || oa /= sid) = prns
  ifPrns _ _ _ _ = id
  prec = \case
    Lit _                        -> Nothing
    Prefix  (Operator s r _) _   -> Just (s,r)
    Postfix (Operator s r _) _   -> Just (s,r)
    Binary  (Operator s r _) _ _ -> Just (s,r)
{-# INLINABLE showExpr #-}
