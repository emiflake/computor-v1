{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SolveM where

import Prettyprinter
import Prettyprinter.Render.Terminal

import qualified Data.Text as Text
import Data.Text (Text)

import Prelude hiding (log)

import qualified  Report
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
  = DivideBy0 Tag.Span Tag.Span Tag.Span -- Solving failed because divide by 0 was encountered somewhere along the way
  | ExponentTooLarge Tag.Span
  | ShapeError Tag.Span
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
    Report.prettyIndent 4 (annotate (color Yellow) (pretty cs))
    

-- Prettify the error message
prettyError :: Text -> Equation -> SolveError -> Doc AnsiStyle
prettyError source equation = \case
  ExponentTooLarge span@(Tag.Span (Tag.Position l c) (Tag.Position _ c')) ->
    vsep
    ["Exponent is too large at " <> pretty span
    , ""
    , "Starting at line" <+> pretty l <+> "column" <+> pretty c <> ":"
    , Report.prettySpanSquiggly 4 span source
    , annotate (color Black) "NOTE: This is because in computor-v1, exponents of degrees higher than 3 are not required to be solved"
    , ""
    ]
  ShapeError span@(Tag.Span (Tag.Position l c) (Tag.Position _ c')) ->
    vsep
    [ "Equation is of wrong shape at " <> pretty span
    , ""
    , "Starting at line" <+> pretty l <+> "column" <+> pretty c <> ":"
    , Report.prettySpanSquiggly 4 span source
    , annotate (color Black) "NOTE: This usually happens because reduction isn't smart enough to reorder terms, try moving some terms to left-hand side"
    , ""
    ]
  (DivideBy0 span@(Tag.Span (Tag.Position l c) (Tag.Position _ c')) lhs rhs) ->
    vsep
    ["Reduction error caused by divide by 0 at " <> pretty span
    , ""
    , indent 6 "When dividing"
    , Report.prettyIndent 4 (annotate (color Yellow) (pretty (Expr.smallestContainingSpan lhs equation)))
    , "by " <+> annotate (color Blue) "|"
    , Report.prettyIndent 4 (annotate (color Yellow) (pretty (Expr.smallestContainingSpan rhs equation)) <+> "... which evaluates to 0")
    , ""
    , "Starting at line" <+> pretty l <+> "column" <+> pretty c <> ":"
    , Report.prettySpanSquiggly 4 span source
    ]
  
