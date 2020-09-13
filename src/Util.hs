module Util where


import Control.Monad.State

-- import qualified Data.List.NonEmpty as NonEmpty
-- import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
-- import Prettyprinter.Render.Terminal
-- import qualified Tag
import qualified Expr
import Expr (Expr', Expr, Equation)
-- import Prettyprinter
-- import qualified Reduce
-- import qualified Solve
import SolveM
import Solve.Reduce

-- Utility functions for GHCi

lemma :: SolveM ()
lemma = do
  putCurrentState
  operateOnSide Both (reduce1 >=> reorder >=> reduce1)
  putCurrentState

solveAST :: Text -> IO ()
solveAST text =
  case Expr.runParserE Expr.equation text of
    Right eq ->
      runSolveM eq lemma >>= print
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
