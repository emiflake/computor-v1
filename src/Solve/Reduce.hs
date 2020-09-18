{-# LANGUAGE LambdaCase #-}
module Solve.Reduce where

import Debug.Trace

import Prelude hiding (span)

import Control.Applicative
import Control.Monad.Except

import Prettyprinter
import Prettyprinter.Render.Terminal

import Expr
import qualified Tag

import SolveM

(~~=) :: Double -> Double -> Bool
(~~=) x y = abs (x - y) <= 0.00001

isZero :: Expr' -> Bool
isZero (LitNum x) = 0 ~~= x
isZero _ = False


runOp :: Expr.Op -> Double -> Double -> Double
runOp op a b =
  case op of
    Expr.Add -> a + b
    Expr.Sub -> a - b
    Expr.Mul -> a * b
    Expr.Div -> a / b
    Expr.Pow -> a ** b

reorder :: Expr -> SolveM Expr
reorder =
  pure . Expr.fromExprL . Expr.toExprL

reduce1 :: Expr -> SolveM Expr
reduce1 = \case
  a@(Tag.At span (Expr.BinOp op lhs rhs)) -> do
    candidate <- liftA2 (,) (reduce1 lhs) (reduce1 rhs)
    case (op, candidate) of
      -- +0 or -0
      (_, ((Tag.At _ (Expr.LitNum 0)), rhs')) | op `elem` [Expr.Add] ->
        pure $ rhs'

      (_, (lhs', (Tag.At _ rhs'))) | op `elem` [Expr.Add, Expr.Sub] && isZero rhs' ->
        pure $ lhs'

      -- ^1
      (Expr.Pow, (lhs', (Tag.At _ (Expr.LitNum 1)))) ->
        pure $ lhs'

      -- ^0
      -- (Expr.Pow, (_, (Tag.At _ (Expr.LitNum 0)))) ->
      --   pure $ Tag.At span (Expr.LitNum 1)

      -- /1
      (Expr.Div, (lhs', (Tag.At _ (Expr.LitNum 1)))) ->
        pure $ lhs'

      -- *1
      (Expr.Mul, ((Tag.At _ (Expr.LitNum 1)), rhs')) ->
        reduce1 $ rhs'

      (Expr.Mul, (lhs', (Tag.At _ (Expr.LitNum n)))) | n ~~= 1 ->
        reduce1 $ lhs'

      -- *0
      (Expr.Mul, ((Tag.At _ lhs'), (Tag.At _ rhs')))
        | any isZero [rhs', lhs'] ->
        pure $ Tag.At span (Expr.LitNum 0)

      -- /0
      (_, (Tag.At lhsSpan _, Tag.At rhsSpan (Expr.LitNum 0))) | op == Expr.Div ->
        -- FIXME: `a` here represents a subexpression, not the entire expression, so it will get represented badly
        throwError $ DivideBy0 span lhsSpan rhsSpan

      -- X - X
      (Expr.Sub, (lhs', rhs')) | lhs' ~= rhs' ->
        pure $ Tag.At span (Expr.LitNum 0)

      -- X * X
      (Expr.Mul, (lhs', rhs')) | lhs' ~= rhs' ->
        reduce1 $ Tag.At span (Expr.BinOp Expr.Pow lhs' (Tag.At span (Expr.LitNum 2)))

      -- X + X
      (Expr.Add, (lhs', rhs')) | lhs' ~= rhs' ->
        reduce1 $ Tag.At span (Expr.BinOp Expr.Mul lhs' (Tag.At span (Expr.LitNum 2)))

      -- X / n = 1 / n * X, optional, causes floating point precision loss
      (Expr.Div, ((Tag.At s (FreeVar i)), (Tag.At s' (LitNum d)))) ->
        reduce1 $ Tag.At span (Expr.BinOp Expr.Mul (Tag.At s' (LitNum (1/d))) (Tag.At s (FreeVar i)))

      -- Multiplication transitivity
      (_, ( Tag.At _  (Expr.BinOp Expr.Mul (Tag.At _ (Expr.LitNum ma)) v)
          , Tag.At _ (Expr.BinOp Expr.Mul (Tag.At _ (Expr.LitNum mb)) v')))
        | v ~= v' ->
          reduce1 $ Tag.At span (Expr.BinOp Expr.Mul (Tag.At span (Expr.LitNum (runOp op ma mb))) v)

      (_, ( v, Tag.At _ (Expr.BinOp Expr.Mul (Tag.At _ (Expr.LitNum mb)) v')))
        | v ~= v' ->
          reduce1 $ Tag.At span (Expr.BinOp Expr.Mul (Tag.At span (Expr.LitNum (runOp op 1 mb))) v')

      (_, (Tag.At _ (Expr.LitNum lhs'), Tag.At _ (Expr.LitNum rhs'))) ->
        fmap (Tag.At span . Expr.LitNum) $
           pure $ runOp op lhs' rhs'

      (_, (lhs', rhs')) ->
        pure $ Tag.At span (Expr.BinOp op lhs' rhs')
  a -> pure a
