{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Foldable
import Control.Applicative
import Control.Monad (void)
import Data.Functor

import Control.Lens

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

data ExprL'
  = FreeVarL (Identifier 'FreeScope)
  | LitNumL  Double
  | BinOpL Op (NonEmpty ExprL)
  deriving (Show, Eq)

instance Ord ExprL' where
  LitNumL _ <= _ = True 
  FreeVarL _ <= FreeVarL _ = True
  FreeVarL _ <= BinOpL _ _ = True
  _ <= _ = False

toExprL :: Expr -> ExprL
toExprL = fmap \case
  FreeVar i -> FreeVarL i
  LitNum  d -> LitNumL  d
  -- FIXME: reduce duplication
  BinOp   Add lhs rhs ->
    case (toExprL lhs, toExprL rhs) of
      (Tag.At _ (BinOpL Add lhs'), Tag.At _ (BinOpL Add rhs')) -> BinOpL Add (lhs' <> rhs')
      (Tag.At _ (BinOpL Add lhs'), rhs') -> BinOpL Add (lhs' <> (rhs' :| []))
      (lhs', Tag.At _ (BinOpL Add rhs')) -> BinOpL Add (lhs' NonEmpty.<| rhs')
      (lhs', rhs') -> BinOpL Add (lhs' :| [rhs'])
  BinOp   Mul lhs rhs ->
    case (toExprL lhs, toExprL rhs) of
      (Tag.At _ (BinOpL Mul lhs'), Tag.At _ (BinOpL Mul rhs')) -> BinOpL Mul (lhs' <> rhs')
      (Tag.At _ (BinOpL Mul lhs'), rhs') -> BinOpL Mul (lhs' <> (rhs' :| []))
      (lhs', Tag.At _ (BinOpL Mul rhs')) -> BinOpL Mul (lhs' NonEmpty.<| rhs')
      (lhs', rhs') -> BinOpL Mul (lhs' :| [rhs'])
  BinOp   op lhs rhs ->
    BinOpL op (toExprL lhs :| [toExprL rhs])

fromExprL :: ExprL -> Expr
fromExprL (Tag.At span exprL) = case exprL of
  FreeVarL i -> Tag.At span $ FreeVar i
  LitNumL d -> Tag.At span $ LitNum d
  BinOpL op (NonEmpty.sort -> h :| t) | op `elem` [Add, Mul] -> 
    foldl (\acc v -> Tag.At span $ BinOp op acc (fromExprL v)) (fromExprL h) t
  BinOpL op (h :| t) -> 
    foldl (\acc v -> Tag.At span $ BinOp op acc (fromExprL v)) (fromExprL h) t

(~=) :: Expr -> Expr -> Bool
(Tag.At _ (FreeVar v)) ~= (Tag.At _ (FreeVar v')) = v == v'
(Tag.At _ (LitNum d)) ~= (Tag.At _ (LitNum d')) = d == d'
(Tag.At _ (BinOp op lhs rhs)) ~= (Tag.At _ (BinOp op' lhs' rhs')) = op == op' && lhs ~= lhs && rhs ~= rhs
_ ~= _ = False

line :: Int -> Expr -> Maybe Expr
line n full@(Tag.At exprSpan e) =
  if Tag.fullyInLine n exprSpan
  then
    Just full
  else
    case e of
      (BinOp _ lhs rhs) -> Expr.line n lhs <|> Expr.line n rhs
      a -> Nothing

smallestContainingSpan :: Tag.Span -> Expr -> Maybe Expr
smallestContainingSpan span full@(Tag.At exprSpan e) =
  if span == exprSpan
  then
    Just full
  else
    case e of
      (BinOp _ lhs rhs) -> smallestContainingSpan span lhs <|> smallestContainingSpan span rhs
      a -> Nothing
  
-- PARSE

{-| INVARIANT: free identifiers are only represented by capital X
-}
freeIdent :: Parser (Identifier 'FreeScope)
freeIdent = identifier @'FreeScope ['X'] []

whitespace :: Parser ()
whitespace = void $ many (oneOf [' ', '\n', '\r', '\t'])

identifier :: forall (scope :: Scope). [Char] -> [Char] -> Parser (Identifier scope)
identifier beginning rest =
  fmap Identifier $ liftA2 Text.cons (oneOf beginning) (fmap Text.pack $ many (oneOf rest))

litNum :: Parser Double
litNum = do
  negation <- string "-" <|> pure ""
  intPart <- many1 digit
  floatPart <- try (do
          dot <- string "."
          floatPart <- many1 digit
          pure $ dot <> floatPart
      ) <|> pure ""
  pure . read $ concat [ negation, intPart, floatPart ]

operator :: [Char] -> Op -> Parser Op
operator syntax result =
  (string syntax) >> pure result

opLevelLeft :: Parser Op -> Parser Expr -> Parser Expr
opLevelLeft op descend =
  let
    helper :: Expr -> Parser Expr
    helper lhs =
      asum
      [ try $ do
          _ <- whitespace
          op' <- op
          _ <- whitespace
          rhs <- descend
          let span = Tag.mergeSpans (view Tag.span lhs) (view Tag.span rhs)
          helper (Tag.At span (BinOp op' lhs rhs))
      , pure lhs
      ]
  in
    helper =<< descend

opLevelRight :: Parser Op -> Parser Expr -> Parser Expr
opLevelRight op descend =
  descend >>= \lhs ->
    asum
    [ try $ do
        _ <- whitespace
        op' <- op
        _ <- whitespace
        rhs <- descend
        pure $ Tag.spannedBy lhs rhs (BinOp op' lhs rhs)
    , pure lhs
    ]

term :: Parser Expr
term =
  asum
  [ Tag.spanned $ FreeVar <$> freeIdent
  , Tag.spanned $ LitNum <$> litNum
  , char '(' *> whitespace *> expr <* whitespace <* char ')'
  ]

expr'' :: Parser Expr
expr'' = opLevelLeft (asum [ operator "^" Pow ]) term

expr' :: Parser Expr
expr' = opLevelLeft (asum [ operator "*" Mul, operator "/" Div ]) expr''

expr :: Parser Expr
expr = opLevelLeft (asum [ operator "+" Add, operator "-" Sub ]) expr'

equation :: Parser Equation
equation =
  Equation
    <$  whitespace
    <*> (expr <?> "lefthand side of equation")
    <*  whitespace
    <*  char '='
    <*  whitespace
    <*> (expr <?> "righthand side of equation")
    <*  whitespace

input :: Parser Equation
input = equation <* eof

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
    LitNum  double -> pretty double
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
    BinOpL op terms ->
      "(" <> (align . sep . zipWith (<>) (emptyDoc : repeat (pretty op <> " ")) . fmap pretty $ NonEmpty.toList terms) <> ")"
