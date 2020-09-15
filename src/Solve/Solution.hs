{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Solve.Solution where

import Debug.Trace

import Prettyprinter
import Prettyprinter.Render.Terminal

import Data.Maybe

import Expr
import SolveM
import qualified Tag

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad.State

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Solve.Tactics

import Solve.Reduce

data PolyTerm
  = PolyTerm
  { _nMag :: Double
  , _nXPower :: Double
  }
  deriving (Show, Eq)


type PolyTermEquation = Map Double Double

instance Pretty PolyTerm where
  pretty (PolyTerm mag power) = pretty mag <+> "X^" <> pretty power

data Solvable
  = Degree0 Double
  | Degree1 Double Double
  | Degree2 Double Double Double
  deriving (Show, Eq)

solve :: Solvable -> Solution
solve = \case
  Degree0 n | n ~~= 0 -> AllValues
  Degree0 n -> NoValues
  Degree1 a b -> OneValue (negate a / b)
  Degree2 a b c ->
    let discriminant = b ** 2 - 4 * a * c
    in
    if discriminant > 0
    then TwoRoots discriminant
                  ((-b + sqrt discriminant) / (2 * a))
                  ((-b - sqrt discriminant) / (2 * a))
    else if discriminant < 0
    then NoValues
    else OneValue ((-b) / (2 * a))
      

data Solution
  = TwoRoots Double Double Double
  | OneValue Double
  | AllValues
  | NoValues
  deriving (Show, Eq)

prettySolution solution =
  case solution of
    NoValues -> annotate (color Red) "No valid X exists to fulfil this equation" <> hardline
    AllValues -> annotate (color Green) "All values for X would fulfil this equation" <> hardline
    OneValue v -> "Solved equation:" <+> annotate (color Green) ("X = " <> pretty v) <> hardline
    TwoRoots d x x' -> "Solved equation, the discriminant is" <+>
                       annotate (color Green) (pretty d) <+>
                       "solutions are" <+> annotate (color Green)("X =" <+> pretty x) <+>
                       "and" <+> annotate (color Green) ("X =" <+> pretty x') <> hardline

data PolyTermError
  = ExponentTooLarge Tag.Span
  | ShapeError Tag.Span
  deriving (Show, Eq)

toTerm :: Expr -> Either PolyTermError PolyTerm
toTerm (Tag.At span expr) =
  case expr of
    LitNum d -> Right $ PolyTerm d 0
    FreeVar i -> Right $ PolyTerm 1 1
    BinOp Mul (Tag.At _ (LitNum d)) (Tag.At _ rhs) ->
      case rhs of
        FreeVar i -> Right $ PolyTerm d 1
        BinOp Pow (Tag.At _ (FreeVar _)) (Tag.At s (LitNum mag)) ->
          if mag <= 2 && mag >= 0 then Right $ PolyTerm d mag
          else Left $ ExponentTooLarge s
        _ -> Left $ ShapeError span
    BinOp Pow (Tag.At _ (FreeVar _)) (Tag.At s (LitNum mag)) ->
      if mag <= 2 && mag >= 0 then Right $ PolyTerm 1 mag
      else Left $ ExponentTooLarge s
    _ ->
      Left $ ShapeError span

toPolyTermEquation :: [PolyTerm] -> PolyTermEquation
toPolyTermEquation [] = Map.empty
toPolyTermEquation (PolyTerm mag power:xs) =
  foldl (\acc (PolyTerm mag power) -> Map.insertWith (+) power mag acc) (Map.singleton power mag) xs

accumulateTerms :: SolveM PolyTermEquation
accumulateTerms = do
  -- SolveM.log $ "Accumulating terms" <> hardline
  Equation (toExprL -> lhs) rhs <- get
  case introduceAdd lhs of
    Tag.At _ (AppL Add terms) -> do
      case traverse (toTerm . fromExprL) terms of
        Left e -> do
          -- SolveM.log $ pretty (show e) <> hardline
          pure Map.empty
        Right nonEmpty -> do
          SolveM.log $ "PolyTerms:" <+> pretty nonEmpty <> hardline
          pure . toPolyTermEquation $ NonEmpty.toList nonEmpty
    _ -> do
      -- SolveM.log $ "Top level isn't addition" <> hardline
      pure Map.empty


identifySolvable :: SolveM (Maybe Solvable)
identifySolvable = do
  equation <- accumulateTerms
  let greatest = maximum (Map.keys equation)
  pure $ case greatest of
    2.0 ->
      let c = fromMaybe 0 $ Map.lookup 0.0 equation
          b = fromMaybe 0 $ Map.lookup 1.0 equation
          a = fromMaybe 0 $ Map.lookup 2.0 equation
      in Just $ Degree2 a b c
    1.0 ->
      let a = fromMaybe 0 $ Map.lookup 0.0 equation
          b = fromMaybe 0 $ Map.lookup 1.0 equation
      in Just $ Degree1 a b
    0.0 ->
      let a = fromMaybe 0 $ Map.lookup 0.0 equation
      in Just $ Degree0 a
    _ -> Nothing
