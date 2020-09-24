{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Solve.Solution where

import Prettyprinter
import Prettyprinter.Render.Terminal
import Report

import Data.Maybe

import Expr
import SolveM
import qualified Tag

import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad.Except
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

normalizeZero :: Double -> Double
normalizeZero n | n == 0 = 0
normalizeZero n = n

prettySolution :: Solvable -> Solution -> Doc AnsiStyle
prettySolution solvable solution =
  let prettySolution =
        case solution of
          NoValues -> annotate (color Red) "No valid X exists to fulfil this equation" <> hardline
          AllValues -> annotate (color Green) "All values for X would fulfil this equation" <> hardline
          OneValue (normalizeZero -> v) ->
            "Solved equation:" <+> annotate (color Green) ("X = " <> pretty v) <> hardline
          TwoRoots (normalizeZero -> d) (normalizeZero -> x) (normalizeZero -> x') ->
            "Solved equation, the discriminant is" <+>
            annotate (color Green) (pretty d) <+>
            "solutions are" <+> annotate (color Green)("X =" <+> pretty x) <+>
            "and" <+> annotate (color Green) ("X =" <+> pretty x') <> hardline

      prettySolvable =
        case solvable of
          Degree0 _ -> "Equation of degree" <+> annotate (color Blue) "0"
          Degree1 _ _ ->  "Equation of degree" <+> annotate (color Blue) "1"
          Degree2 _ _ _ -> "Equation of degree" <+> annotate (color Blue) "2"
  in
  vsep
  [ prettySolvable
  , prettySolution
  ]

toTerm :: Expr -> SolveM PolyTerm
toTerm (Tag.At span expr) =
  case expr of
    LitNum d -> pure $ PolyTerm d 0
    FreeVar i -> pure $ PolyTerm 1 1
    BinOp Mul (Tag.At _ (LitNum d)) (Tag.At _ rhs) ->
      case rhs of
        FreeVar i -> pure $ PolyTerm d 1
        BinOp Pow (Tag.At _ (FreeVar _)) (Tag.At s (LitNum mag)) ->
          if mag <= 2 && mag >= 0 then pure $ PolyTerm d mag
          else throwError $ ExponentTooLarge s
        _ -> throwError $ ShapeError span
    BinOp Pow (Tag.At _ (FreeVar _)) (Tag.At s (LitNum mag)) ->
      if mag <= 2 && mag >= 0 then pure $ PolyTerm 1 mag
      else throwError $ ExponentTooLarge s
    _ ->
      throwError $ ShapeError span

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
      res <- traverse (toTerm . fromExprL) terms
      -- SolveM.log $ "PolyTerms:" <+> pretty nonEmpty <> hardline
      pure . toPolyTermEquation $ NonEmpty.toList res
    _ -> do
      -- SolveM.log $ "Top level isn't addition" <> hardline
      pure Map.empty


identifySolvable :: SolveM (Maybe Solvable)
identifySolvable = do
  equation <- accumulateTerms
  let greatest = foldl max (-1) (Map.keys equation)
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

getSolution :: SolveM (Maybe (Solvable, Solution))
getSolution = do
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
