{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SolveM where

import Prettyprinter
import Prettyprinter.Render.Terminal

import Prelude hiding (log)

import Expr
import qualified Tag

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except

-- Solve Monad
-- Rule-based system for solution with steps in between being optionally reported with pretty log

data SolveError
  = DivideBy0 Expr Tag.Span Tag.Span Tag.Span -- Solving failed because divide by 0 was encountered somewhere along the way
  deriving (Show, Eq)

-- Stack explanation:
--   Writer -> Log = [Doc ann]
--   Reader -> Source 'Equation' AST
--   State  -> Current Equation
--   Except -> Throw execution error

-- TODO: Possible idea:
-- newtype SolveM c a
--   where `c` is the current candidate, and provide traversal functions, such that you can switch from candidate
--   e.g. Equation -> EquationL
--   this allows for more flexibility with creating tactics and cleaner code.
--   Possibly makes this entire thing extensible to ComputorV2

newtype SolveM a =
  SolveM
  { runSolveM' :: ExceptT SolveError (StateT Equation (WriterT (Doc AnsiStyle) (ReaderT Equation IO))) a
  }
  deriving
    ( Monad
    , Functor
    , Applicative
    , MonadIO
    , MonadReader Equation
    , MonadError SolveError
    , MonadWriter (Doc AnsiStyle)
    , MonadState Equation
    )

-- Low level operations
-- Shouldn't emit anything by themselves, they are the building blocks for other parts

runSolveM :: Equation -> SolveM a -> IO (Either SolveError a)
runSolveM equation f = do
  (result, logs) <- (`runReaderT` equation) . runWriterT . (`evalStateT` equation) . runExceptT . runSolveM' $ f
  putDoc logs
  pure result

log :: Doc AnsiStyle -> SolveM ()
log = tell

data Side = LHS | RHS | Both

operateOnSide :: Side -> (Expr -> SolveM Expr) -> SolveM ()
operateOnSide side f = do
  Equation lhs rhs <- get
  put =<< case side of
            LHS -> Equation <$> (f lhs) <*> pure rhs
            RHS -> Equation lhs <$> (f rhs)
            Both -> Equation <$> (f lhs) <*> (f rhs)

putCurrentState :: SolveM ()
putCurrentState =
  get >>= \cs -> log $
    hardline
    <> indent 8 (annotate (color Blue) (pretty cs)) <> hardline
    <> hardline


-- Higher level operations

-- Emits an 'arrow' to pretty log
-- reductionStep :: (Equation -> SolveM Equation) -> SolveM ()

-- reduce :: Side -> SolveM ()
-- reduce side = operateOnSide side (Reduce.reduce

-- reorder :: SolveM ()

-- isSolvable :: SolveM Bool

-- solve :: SolveM Solution

-- Prettify the error message

prettyError (DivideBy0 srcExpr span@(Tag.Span (Tag.Position l c) _) lhs rhs) =
    "Reduction error caused by divide by 0 at " <> pretty span <> hardline
    <> hardline
    <> "When dividing" <> hardline
    <> hardline
    <> indent 4 (annotate (color Blue) (pretty (Expr.smallestContainingSpan lhs srcExpr))) <> hardline
    <> hardline
    <> "by" <> hardline
    <> hardline
    <> indent 4 (annotate (color Blue) (pretty (Expr.smallestContainingSpan rhs srcExpr))) <+> "... which evaluates to 0" <> hardline
    <> hardline
    <> hardline <>
    (case Expr.line l srcExpr of
      Just line ->
        "starting on line" <+> pretty l <+> "column" <+> pretty c <> ":" <> hardline
        <> hardline
        <> (indent 4 (Expr.prettyAnnotateSpan span (color Red) line)) <> hardline
      Nothing -> 
        emptyDoc
    )
