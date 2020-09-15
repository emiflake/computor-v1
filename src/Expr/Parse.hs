{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Expr.Parse where

import qualified Tag

import qualified Data.Text as Text
import Data.Text (Text)

import Text.Parsec.Text
import Text.Parsec hiding ((<|>), many)

import Data.Foldable
import Control.Applicative
import Control.Monad

import Expr

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
          let span = Tag.span lhs <> Tag.span rhs
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
