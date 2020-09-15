{-# LANGUAGE LambdaCase #-}
module Solve.Tactics where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State

import Prettyprinter
import Prettyprinter.Render.Terminal

import Expr
import qualified Tag

import SolveM


introduceAdd :: ExprL -> ExprL
introduceAdd =
  \case
    a@(Tag.At _ (Expr.AppL Expr.Add vs)) -> a
    a@(Tag.At s i) -> Tag.At s (Expr.AppL Expr.Add (a :| []))

-- Moves all terms from rhs to lhs of equation
allToLeft :: SolveM ()
allToLeft = do
  Equation lhs rhs <- get
  let (Tag.At s (AppL Add lhs'), (Tag.At _ (AppL Add rhs'))) = (introduceAdd $ toExprL lhs, introduceAdd $ toExprL rhs)
  let (EquationL lhs'' rhs'') = allToLeft' s (lhs', rhs')
  put $ Equation (fromExprL lhs'') (fromExprL rhs'')

  where

    negExpr :: ExprL -> ExprL
    negExpr e@(Tag.At s _) = Tag.At s (Expr.AppL Expr.Mul (e :| [Tag.At s (Expr.LitNumL (-1.0))]))

    allToLeft' :: Tag.Span -> (NonEmpty ExprL, NonEmpty ExprL) -> Expr.EquationL
    allToLeft' s (lhs@(x :| xs), y :| ys) =
      let
        finalized =
            (x :| xs) <> fmap negExpr (y :| ys)
      in
      Expr.EquationL
        (Tag.At s (Expr.AppL Expr.Add finalized))
        (Tag.At (foldl1 Tag.mergeSpans $ fmap Tag.unspan (y :| ys)) (Expr.LitNumL 0))

