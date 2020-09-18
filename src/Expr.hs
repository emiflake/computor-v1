{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Expr where

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))

import Control.Applicative

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Text

import qualified Tag as Tag

import Prettyprinter

-- EXPR

data Equation =
  Equation Expr Expr
  deriving (Show, Eq)

data EquationL =
  EquationL ExprL ExprL
  deriving (Show, Eq)

data Scope = TermScope | TypeScope | FreeScope

newtype Identifier (scope :: Scope) = Identifier Text
  deriving (Show, Ord, Eq)

data Op = Add | Sub | Mul | Div | Pow
  deriving (Show, Ord, Eq)

type Expr = Tag.Spanned Expr'

data Expr'
  = FreeVar (Identifier 'FreeScope)
  | LitNum  Double
  | BinOp   Op Expr Expr
  deriving (Show, Ord, Eq)

type ExprL = Tag.Spanned ExprL'

-- TODO: rename away from AppL, it's clearly not a *binary operation* anymore
data ExprL'
  = FreeVarL (Identifier 'FreeScope)
  | LitNumL  Double
  | AppL Op (NonEmpty ExprL)
  deriving (Show, Eq)

instance Ord ExprL' where
  LitNumL _ <= _ = True
  FreeVarL _ <= FreeVarL _ = True
  FreeVarL _ <= AppL _ _ = True
  _ <= _ = False

toExprL :: Expr -> ExprL
toExprL = fmap \case
  FreeVar i -> FreeVarL i
  LitNum  d -> LitNumL  d
  -- FIXME: reduce duplication
  BinOp   Add lhs rhs ->
    case (toExprL lhs, toExprL rhs) of
      (Tag.At _ (AppL Add lhs'), Tag.At _ (AppL Add rhs')) -> AppL Add (lhs' <> rhs')
      (Tag.At _ (AppL Add lhs'), rhs') -> AppL Add (lhs' <> (rhs' :| []))
      (lhs', Tag.At _ (AppL Add rhs')) -> AppL Add (lhs' NonEmpty.<| rhs')
      (lhs', rhs') -> AppL Add (lhs' :| [rhs'])
  BinOp   Mul lhs rhs ->
    case (toExprL lhs, toExprL rhs) of
      (Tag.At _ (AppL Mul lhs'), Tag.At _ (AppL Mul rhs')) -> AppL Mul (lhs' <> rhs')
      (Tag.At _ (AppL Mul lhs'), rhs') -> AppL Mul (lhs' <> (rhs' :| []))
      (lhs', Tag.At _ (AppL Mul rhs')) -> AppL Mul (lhs' NonEmpty.<| rhs')
      (lhs', rhs') -> AppL Mul (lhs' :| [rhs'])
  BinOp   op lhs rhs ->
    AppL op (toExprL lhs :| [toExprL rhs])

fromExprL :: ExprL -> Expr
fromExprL (Tag.At span exprL) = case exprL of
  FreeVarL i -> Tag.At span $ FreeVar i
  LitNumL d -> Tag.At span $ LitNum d
  AppL op (NonEmpty.sort -> h :| t) | op `elem` [Add, Mul] -> 
    foldl (\acc v -> Tag.At span $ BinOp op acc (fromExprL v)) (fromExprL h) t
  AppL op (h :| t) -> 
    foldl (\acc v -> Tag.At span $ BinOp op acc (fromExprL v)) (fromExprL h) t


-- Equal by ignoring spans
(~=) :: Expr -> Expr -> Bool
(Tag.At _ (FreeVar v)) ~= (Tag.At _ (FreeVar v')) = v == v'
(Tag.At _ (LitNum d)) ~= (Tag.At _ (LitNum d')) = d == d'
(Tag.At _ (BinOp op lhs rhs)) ~= (Tag.At _ (BinOp op' lhs' rhs')) = op == op' && lhs ~= lhs' && rhs ~= rhs'
_ ~= _ = False

line :: Int -> Equation -> Maybe Expr
line n (Equation lhs rhs) =
  go n lhs <|> go n rhs
  where
  go :: Int -> Expr -> Maybe Expr
  go n' full@(Tag.At exprSpan e) =
    if Tag.fullyInLine n' exprSpan
    then
      Just full
    else
      case e of
        (BinOp _ lhs' rhs') -> go n' lhs' <|> go n' rhs'
        _ -> Nothing


smallestContainingSpan :: Tag.Span -> Equation -> Maybe Expr
smallestContainingSpan span (Equation lhs rhs) =
  go span lhs <|> go span rhs
  where
    go :: Tag.Span -> Expr -> Maybe Expr
    go span full@(Tag.At exprSpan e) =
      if span == exprSpan
      then
        Just full
      else
        case e of
          (BinOp _ lhs' rhs') -> go span lhs' <|> go span rhs'
          _ -> Nothing

-- RUN

runParserE :: Parser a -> Text -> Either ParseError a
runParserE p =
  runParser p () ""

-- PRETTY

instance Pretty Op where
  pretty =
    pretty @Text . \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Pow -> "^"

instance forall scope. Pretty (Identifier scope) where
  pretty (Identifier ident) = pretty ident

instance Pretty Expr' where
  pretty = \case
    FreeVar ident -> pretty ident
    LitNum  double ->
      if fromInteger (round double) == double then
        pretty (round double :: Int)
      else
        pretty double
    BinOp   Mul lhs@(Tag.At _ (LitNum _)) (Tag.At _ (FreeVar rhs)) -> pretty lhs <> pretty rhs
    BinOp   op lhs rhs -> "(" <> pretty lhs <+> pretty op <+> pretty rhs <> ")"

prettyAnnotateSpan :: Tag.Span -> ann -> Expr -> Doc ann
prettyAnnotateSpan span ann full@(Tag.At exprSpan e) =
  if span == exprSpan
  then
    annotate ann (pretty full)
  else
    case e of
      BinOp   op lhs rhs -> "(" <> prettyAnnotateSpan span ann lhs <+> pretty op <+> prettyAnnotateSpan span ann rhs <> ")"
      a -> pretty a

prettyAnnotateSpanEquation :: Tag.Span -> ann -> Equation -> Doc ann
prettyAnnotateSpanEquation span ann (Equation lhs rhs) =
  prettyAnnotateSpan span ann lhs <> softline <> "=" <> softline <> prettyAnnotateSpan span ann rhs

instance Pretty Equation where
  pretty (Equation lhs rhs) =
    pretty lhs <> softline <> "=" <> softline <> pretty rhs

instance Pretty EquationL where
  pretty (EquationL lhs rhs) =
    pretty lhs <> softline <> "=" <> softline <> pretty rhs

instance Pretty ExprL' where
  pretty = \case
    FreeVarL ident -> pretty ident
    LitNumL  double -> pretty double
    AppL op terms ->
      "(" <> (align . sep . zipWith (<>) (emptyDoc : repeat (pretty op <> " ")) . fmap pretty $ NonEmpty.toList terms) <> ")"
