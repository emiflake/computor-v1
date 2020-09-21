{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

import Config

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

solveEquationMay :: Config -> Equation -> Maybe (Solvable, Solution)
solveEquationMay cfg equation =
  case runSolveM cfg equation getSolution of
    (Right v, _) -> v
    (Left e, _) -> Nothing

solveEquationErr :: Config -> Equation -> Maybe SolveError
solveEquationErr cfg equation =
  case runSolveM cfg equation getSolution of
    (Right v, _) -> Nothing
    (Left e, _) -> Just e

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
      case runSolveM cfg eq getSolution of
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
