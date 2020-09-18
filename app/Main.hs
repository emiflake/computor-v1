{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where


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

import Options.Applicative
import Config

main :: IO ()
main =
  execParser opts >>= solveAST

lemma :: SolveM (Maybe (Solvable, Solution))
lemma = do
  SolveM.log . annotate (color Black) $ Report.boxed "Using simple solution algorithm"
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
  pure $ fmap (\s' -> (s', solve s')) solvable

solveAST :: Config -> IO ()
solveAST cfg@Config{..} =
  let putPretty =
        if showSteps then
          if showColours then
            putDoc
          else
            print
        else
          const $ pure ()
  in
  case Expr.runParserE equation expression of
    Right eq ->
      runSolveM cfg eq lemma >>= \case
        (Right (Just (solvable, solution)), logs) -> do
          putPretty logs
          putDoc $ prettySolution solvable solution
        (Right Nothing, logs) -> do
          putPretty logs
          putStrLn "No solution was found..."
        (Left e, logs) -> do
          putPretty logs
          putDoc (prettyError expression eq e)
    Left e -> print e
