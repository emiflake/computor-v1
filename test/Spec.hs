{-# LANGUAGE LambdaCase #-}
import Data.Text (Text)

import Test.Hspec
import Lib
import Config
import Data.Maybe
import Expr
import Expr.Parse (equation)
import Solve.Solution
import Text.Printf

cfg :: Config
cfg =
  Config
  { showSteps = False
  , showColours = False
  , expression = ""
  }

succeed :: Expectation
succeed = True `shouldBe` True

shouldNothing :: Maybe a -> Expectation
shouldNothing = \case
  Just v -> expectationFailure "Was Just"
  Nothing -> succeed

shouldJust :: (a -> Expectation) -> Maybe a -> Expectation
shouldJust k = \case
  Just v -> k v
  Nothing -> expectationFailure "Was Nothing"

shouldRight :: (a -> Expectation) -> Either e a -> Expectation
shouldRight k = \case
  Right v -> k v
  Left _ -> expectationFailure "Was Left"

shouldParse :: (Equation -> Expectation) -> Text -> Expectation
shouldParse k source =
  shouldRight k $
    Expr.runParserE equation source

shouldJustParse :: Text -> Expectation
shouldJustParse = shouldParse (const succeed)

shouldSolve :: ((Solvable, Solution) -> Expectation) -> Equation -> Expectation
shouldSolve k equation =
  shouldJust k (solveEquationMay cfg equation)

shouldNotSolve :: Equation -> Expectation
shouldNotSolve equation =
  shouldNothing (solveEquationMay cfg equation)

shouldParseAndSolve :: Text -> Expectation
shouldParseAndSolve =
  shouldParse (shouldSolve (const succeed))

shouldParseAndSolveTo :: (Solution -> Expectation) -> Text -> Expectation
shouldParseAndSolveTo k =
  shouldParse (shouldSolve (k . snd))

shouldParseAndFailSolve :: Text -> Expectation
shouldParseAndFailSolve =
  shouldParse shouldNotSolve

parseTest :: Text -> SpecWith (Arg Expectation)
parseTest source =
  it (printf "parses '%s'" source) $ shouldJustParse source

solveTest :: Text -> SpecWith (Arg Expectation)
solveTest source =
  it (printf "solves '%s'" source) $ shouldParseAndSolve source

solveFailTest :: Text -> SpecWith (Arg Expectation)
solveFailTest source =
  it (printf "does NOT solve '%s'" source) $ shouldParseAndFailSolve source



solutionsApproxEq :: Solution -> Solution -> Expectation
solutionsApproxEq a b =
  case (a, b) of
    (TwoRoots d x y, TwoRoots d' x' y')
      | d ~~= d' && x == x' && y == y' -> succeed
    (OneValue v, OneValue v')
      | v ~~= v' -> succeed
    (AllValues, AllValues) -> succeed
    (NoValues, NoValues) -> succeed
    _ -> expectationFailure (printf "Solutions were not equal %s and %s" (show a) (show b))
  where
    (~~=) :: Double -> Double -> Bool
    (~~=) x y = abs (x - y) <= 0.00001


(==>) :: Text -> Solution -> SpecWith (Arg Expectation)
(==>) source solution =
  it (printf "should solve '%s' to '%s'" source (show solution)) $
    shouldParseAndSolveTo (solutionsApproxEq solution) source


main :: IO ()
main = hspec $ do
  describe "Parse tests" $ do
    parseTest "X = X"
    parseTest "X = X"
    parseTest "2 * X = X"
    parseTest "X = 2 * X"
    parseTest "X * 2 = X"
    parseTest "X = 5 - X"
    parseTest "10 + X = 5 - X"
    parseTest "10 ^ X = 5 - X"
    parseTest "(10 ^ X) ^ 10 = (5 - X - 10)"
    parseTest "(((X))) = 1"

  describe "Solve tests" $ do
    solveTest "X = X"
    solveTest "X = 2 * X"
    solveTest "3 = X"
    solveTest "X / 1 = 3"
    solveTest "X * 1 = 3"

  describe "Solve failure tests" $ do
    solveFailTest "X / 0 = 1"
    solveFailTest "X ^ (0 / 0) = 1"
    solveFailTest "X ^ 3 = 1"
    solveFailTest "X = X + (0 / 0)"
    solveFailTest "X = X / (1 - 1)"
    solveFailTest "X = X * X * X * X * X"

  describe "Solve equality test" $ do
    "X = X" ==> AllValues
    "X = 5" ==> OneValue 5
    "5 = X" ==> OneValue 5
    "5 = 5" ==> AllValues
    "0 = 1" ==> NoValues
    "X * X = 36" ==> TwoRoots 144 (FullySolvable 6) (FullySolvable (-6))
    "X * X + 1 = 37" ==> TwoRoots 144 (FullySolvable 6) (FullySolvable (-6))
    "1 * X * X = 4" ==> TwoRoots 16 (FullySolvable 2) (FullySolvable (-2))
    "-1 = X" ==> OneValue (-1)
    "X - X = X" ==> OneValue 0
    "X = 0.1 * ((5 + 5) * 10 - 1)" ==> OneValue 9.9
