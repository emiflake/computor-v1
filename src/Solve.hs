{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Solve where

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Prettyprinter.Render.Terminal
import qualified Tag
import qualified Expr
import Expr (Expr', Expr, ExprL', ExprL)
import Prettyprinter
import qualified Reduce
import Control.Lens

data Solvable
  = NoFree
  | OneFree
  
introduceAdd :: ExprL -> ExprL
introduceAdd =
  \case
    a@(Tag.At _ (Expr.BinOpL Expr.Add vs)) -> a
    a@(Tag.At s i) -> Tag.At s (Expr.BinOpL Expr.Add (a :| []))

negExpr :: ExprL -> ExprL
negExpr e@(Tag.At s _) = Tag.At s (Expr.BinOpL Expr.Mul (e :| [Tag.At s (Expr.LitNumL (-1.0))]))
  
allToLeft :: Tag.Span -> (NonEmpty ExprL, NonEmpty ExprL) -> Expr.EquationL
allToLeft s (lhs@(x :| xs), y :| ys) =
  let
    finalized =
        (x :| xs) <> fmap negExpr (y :| ys)
  in
  Expr.EquationL
    (Tag.At s (Expr.BinOpL Expr.Add finalized))
    (Tag.At (foldl1 Tag.mergeSpans $ fmap Tag.unspan (y :| ys)) (Expr.LitNumL 0))

findSolvable :: Expr.EquationL -> IO ()
findSolvable (Expr.EquationL lhs rhs) =
  case (introduceAdd lhs, introduceAdd rhs) of
    (Tag.At s (Expr.BinOpL Expr.Add lhs'), Tag.At _ (Expr.BinOpL Expr.Add rhs')) ->
      case Reduce.reduceEquationL $ allToLeft s (lhs', rhs') of
        Left e -> 
          print e
        Right v ->
          putDoc $ "Simplified expression" <> hardline
                <> hardline
                <> indent 8 (annotate (color Green) (pretty v)) <> hardline
                <> hardline
    _ ->
      putStrLn "idk about this one..."
