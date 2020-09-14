{-# LANGUAGE LambdaCase #-}
module Util where

import Control.Monad.State

import Data.Text (Text)

import qualified Expr
import Expr (Expr', Expr, Equation)

import Prettyprinter
import Prettyprinter.Render.Terminal

import SolveM
import Solve.Reduce
import Solve.Tactics
import Solve.Solution

-- Utility functions for GHCi

lemma :: SolveM (Maybe Solution)
lemma = do
  SolveM.log $ "Starting solution tactic 'lemma'" <> hardline
  putCurrentState
  SolveM.log $ "Reduce both sides" <> hardline
  operateOnSide Both $ reduce1 >=> reorder >=> reduce1
  putCurrentState
  SolveM.log $ "Move all terms to rhs" <> hardline
  allToLeft
  putCurrentState
  SolveM.log $ "Reduce both sides" <> hardline
  operateOnSide Both $ reduce1 >=> reorder >=> reduce1
  putCurrentState
  solvable <- identifySolvable
  -- SolveM.log $ pretty (show solvable) <> hardline
  pure $ fmap solve solvable



solveAST :: Text -> IO ()
solveAST text =
  case Expr.runParserE Expr.equation text of
    Right eq ->
      runSolveM eq lemma >>= \case
        Right (Just sol) -> putDoc $ prettySolution sol
        Right Nothing -> putDoc $ "no solution was found..."
        Left e -> putDoc (prettyError e)
    Left e -> print e


-- reduceAST :: Text -> IO ()
  -- reduceAST text =
--   case Expr.runParserE Expr.expr text of
--     Right ast ->
--       case Reduce.reduce1 ast of
--         Left e ->
--           putDoc $ Reduce.showError ast e
--         Right v ->
--           case v of
--             Tag.At _ (Expr.LitNum d) ->
--               putDoc $ "Fully reduced to" <+> annotate (color Green) (pretty d) <> hardline
--             _ ->
--               case Reduce.reduce1 (Expr.fromExprL (Expr.toExprL v)) of
--                 Left e ->
--                   putDoc $ Reduce.showError ast e
--                 Right v' ->
--                     putDoc $ "Could not fully reduce, reduction steps:" <> hardline
--                           <> hardline
--                           <> indent 8 (
--                                annotate (color Blue) (pretty v) <> hardline
--                                <> arrow
--                                <> annotate (color Blue) (pretty (Expr.toExprL v)) <> hardline
--                                <> arrow
--                                <> annotate (color Blue) (pretty (Expr.fromExprL (Expr.toExprL v))) <> hardline
--                                <> arrow
--                                <> annotate (color Blue) (pretty v') <> hardline
--                                <> arrow
--                                <> annotate (color Blue) (pretty (Expr.toExprL v')))
--                           <> hardline
--                           <> hardline
--     Left e ->
--       print e

-- arrow :: Doc ann
-- arrow =
--   hardline <>
--   indent 24 (vsep [ "|" , "v" ]) <> hardline <>
--   hardline

