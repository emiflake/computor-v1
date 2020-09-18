{-# LANGUAGE LambdaCase #-}
module Util where

import Control.Monad.State

import Data.Text (Text)

import Expr
import Expr.Parse

import qualified Report

import Prettyprinter
import Prettyprinter.Render.Terminal

import SolveM
import Solve.Reduce
import Solve.Tactics
import Solve.Solution

-- Utility functions for GHCi

lemma :: SolveM (Maybe Solution)
lemma = do
  SolveM.log $
    annotate (color Black) $ Report.boxed "Using simple solution algorithm"
  putCurrentState
  SolveM.log $ "Reduce both sides" <> hardline
  operateOnSide Both $ reduce1 >=> reorder >=> reduce1
  putCurrentState
  SolveM.log $ "Move all terms to rhs" <> hardline
  allToLeft
  putCurrentState
  SolveM.log $ "Reduce both sides" <> hardline
  operateOnSide Both $ reduce1 >=> reorder >=> reduce1
  operateOnSide Both $ reduce1 >=> reorder >=> reduce1
  putCurrentState
  solvable <- identifySolvable
  pure $ fmap solve solvable



solveAST :: Text -> IO ()
solveAST text =
  case Expr.runParserE equation text of
    Right eq ->
      runSolveM eq lemma >>= \case
        Right (Just sol) -> putDoc $ prettySolution sol
        Right Nothing -> putDoc $ "no solution was found..."
        Left e -> putDoc (prettyError text eq e)
    Left e -> print e

