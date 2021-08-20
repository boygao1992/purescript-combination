module Combination.AST
  ( Expr(..)
  , evalExpr
  ) where

import Prelude
import Control.Monad.ST as Control.Monad.ST
import Control.Monad.ST.Ref as Control.Monad.ST.Ref
import Data.Array as Data.Array
import Data.Array.ST.Iterator as Data.Array.ST.Iterator
import Data.Decimal (Decimal)
import Data.Decimal as Data.Decimal

data Expr
  = Binomial Int Int
  | Sum { from :: Int, to :: Int } (Int -> Expr)
  | Plus Expr Expr
  | Minus Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Min Expr Expr

evalExpr :: Expr -> Decimal
evalExpr = case _ of
  Binomial n k -> nChooseK n k
  Sum x scope -> sum x (evalExpr <<< scope)
  Plus x y -> evalExpr x + evalExpr y
  Minus x y -> evalExpr x - evalExpr y
  Multiply x y -> evalExpr x * evalExpr y
  Divide x y -> evalExpr x / evalExpr y
  Min x y -> Data.Decimal.min (evalExpr x) (evalExpr y)

nChooseK :: Int -> Int -> Decimal
nChooseK n' k' =
  Data.Decimal.factorial n
    / (Data.Decimal.factorial k * Data.Decimal.factorial (n - k))
  where
  n :: Decimal
  n = Data.Decimal.fromInt n'

  k :: Decimal
  k = Data.Decimal.fromInt k'

sum :: { from :: Int, to :: Int } -> (Int -> Decimal) -> Decimal
sum x scope =
  Control.Monad.ST.run do
    result <- Control.Monad.ST.Ref.new (Data.Decimal.fromInt 0)
    indices <-
      Data.Array.ST.Iterator.iterator
        (Data.Array.index (Data.Array.range x.from x.to))
    Data.Array.ST.Iterator.iterate indices \i -> do
      void $ Control.Monad.ST.Ref.modify (_ + scope i) result
    Control.Monad.ST.Ref.read result
