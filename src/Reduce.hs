{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Reduce where

import Control.Applicative

import Prelude hiding (span)
import Debug.Trace as Debug
import qualified Tag
import qualified Expr
import Expr (Expr', Expr, ExprL', ExprL)
import Prettyprinter
import Prettyprinter.Render.Terminal

data ExecutionError
  = DivideBy0 Tag.Span Tag.Span Tag.Span
  deriving (Show, Eq)

showError :: Expr -> ExecutionError -> Doc AnsiStyle
showError srcExpr (DivideBy0 span@(Tag.Span (Tag.Position l c) _) lhs rhs) =
  "Reduction error caused by divide by 0 at " <> pretty span <> hardline
  <> hardline
  <> "When dividing" <> hardline
  <> hardline
  <> indent 4 (annotate (color Blue) (pretty (Expr.smallestContainingSpan lhs srcExpr))) <> hardline
  <> hardline
  <> "by" <> hardline
  <> hardline
  <> indent 4 (annotate (color Blue) (pretty (Expr.smallestContainingSpan rhs srcExpr))) <+> "... which evaluates to 0" <> hardline
  <> hardline
  <> hardline <>
  (case Expr.line l srcExpr of
    Just line ->
      "starting on line" <+> pretty l <+> "column" <+> pretty c <> ":" <> hardline
      <> hardline
      <> (indent 4 (Expr.prettyAnnotateSpan span (color Red) line)) <> hardline
    Nothing -> 
      emptyDoc
  )


-- INVARIANT: It's safe to assume for division, b is non-zero
runOp :: Expr.Op -> Double -> Double -> Double
runOp op a b =
  case op of
    Expr.Add -> a + b
    Expr.Sub -> a - b
    Expr.Mul -> a * b
    Expr.Div -> a / b
    Expr.Pow -> a ** b


-- TODO: Find out how to use Writer Monad for pretty log of reduction steps, which is a bonus for the project.

reduceEquation :: Expr.Equation -> Either ExecutionError Expr.EquationL
reduceEquation (Expr.Equation lhs rhs) =
  liftA2 Expr.EquationL (reduce lhs) (reduce rhs)

reduceEquationL :: Expr.EquationL -> Either ExecutionError Expr.EquationL
reduceEquationL (Expr.EquationL (Expr.fromExprL -> lhs) (Expr.fromExprL -> rhs)) =
  liftA2 Expr.EquationL (reduce lhs) (reduce rhs)


reduce :: Expr -> Either ExecutionError ExprL
reduce e = do
  v <- Reduce.reduce1 e
  v' <- Reduce.reduce1 (Expr.fromExprL (Expr.toExprL v))
  pure (Expr.toExprL v')


-- Simple 2-consideration reduction
reduce1 :: Expr -> Either ExecutionError Expr
reduce1 = \case
  a@(Tag.At span (Expr.BinOp op lhs rhs)) -> do
    candidate <- liftA2 (,) (reduce1 lhs) (reduce1 rhs)
    case (op, candidate) of
      -- +0 or -0
      (_, ((Tag.At _ (Expr.LitNum 0)), rhs')) | op `elem` [Expr.Add, Expr.Sub] ->
        pure $ rhs'

      (_, (lhs', (Tag.At _ (Expr.LitNum 0)))) | op `elem` [Expr.Add, Expr.Sub] ->
        pure $ lhs'

      -- ^1
      (Expr.Pow, (lhs', (Tag.At _ (Expr.LitNum 1)))) ->
        pure $ lhs'

      -- ^0
      (Expr.Pow, (lhs', (Tag.At _ (Expr.LitNum 0)))) ->
        pure $ Tag.At span (Expr.LitNum 1)

      -- /1
      (Expr.Div, (lhs', (Tag.At _ (Expr.LitNum 1)))) ->
        pure $ lhs'

      -- *1
      (Expr.Mul, ((Tag.At _ (Expr.LitNum 1)), rhs')) ->
        reduce1 $ rhs'

      (Expr.Mul, (lhs', (Tag.At _ (Expr.LitNum 1)))) ->
        reduce1 $ lhs'

      -- *0
      (Expr.Mul, ((Tag.At _ lhs'), (Tag.At _ rhs')))
        | Expr.LitNum 0 `elem` [rhs', lhs'] ->
        pure $ Tag.At span (Expr.LitNum 0)

      -- /0
      (_, (Tag.At lhsSpan _, Tag.At rhsSpan (Expr.LitNum 0))) | op == Expr.Div ->
        Left (DivideBy0 span lhsSpan rhsSpan)

      -- X - X
      (Expr.Sub, (lhs', rhs')) | lhs' Expr.~= rhs' ->
        pure $ Tag.At span (Expr.LitNum 0)

      -- X * X
      (Expr.Mul, (lhs', rhs')) | lhs' Expr.~= rhs' ->
        reduce1 $ Tag.At span (Expr.BinOp Expr.Pow lhs' (Tag.At span (Expr.LitNum 2)))

      -- X + X
      (Expr.Add, (lhs', rhs')) | lhs' Expr.~= rhs' ->
        reduce1 $ Tag.At span (Expr.BinOp Expr.Mul lhs' (Tag.At span (Expr.LitNum 2)))

      -- Multiplication transitivity
      (_, (Tag.At s  (Expr.BinOp Expr.Mul (Tag.At _ (Expr.LitNum ma)) v),
       Tag.At s' (Expr.BinOp Expr.Mul (Tag.At _ (Expr.LitNum mb)) v'))) | v Expr.~= v' ->
          reduce1 $ Tag.At span (Expr.BinOp Expr.Mul (Tag.At span (Expr.LitNum (runOp op ma mb))) v)

      (_, (Tag.At _ (Expr.LitNum lhs'), Tag.At _ (Expr.LitNum rhs'))) ->
        fmap (Tag.At span . Expr.LitNum) $
           pure $ runOp op lhs' rhs'

      (_, (lhs', rhs')) ->
        pure $ Tag.At span (Expr.BinOp op lhs' rhs')
  a -> pure a


