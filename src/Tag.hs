{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Tag
  ( Position(..)
  , Span(..)
  , Spanned(..)
  , spanned
  , fullyInLine
  , spannedBy
  ) where

import Prelude hiding (span)

import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Pos as Pos

import Control.Applicative

import Prettyprinter hiding (line)

-- TAGS

data Position
  = Position
  { line :: Int
  , column :: Int
  }
  deriving (Eq, Ord)

instance Show Position where
  show (Position l c) = "[" <> show l <> ":" <> show c <> "]"

data Span =
  Span Position Position
  deriving (Show, Ord, Eq)

data Spanned a
  = At
  { span   :: Span
  , sValue :: a
  }
  deriving (Show, Eq, Functor)

instance Ord a => Ord (Spanned a) where
  compare (At _ a) (At _ b) =
    compare a b

at :: Position -> Position -> a -> Spanned a
at f t =
  At (Span f t)

inLine :: Int -> Position -> Bool
inLine l (Position l' _) = l == l'

fullyInLine :: Int -> Span -> Bool
fullyInLine l (Span f t) = inLine l f && inLine l t

instance Semigroup Span where
  (<>) (Span start _) (Span _ end) =
    Span start end

instance Monoid Span where
  -- Law holds because
  --   forall a. mempty <> a == a && a <> mempty == a
  mempty = Span (Position maxBound maxBound) (Position minBound minBound)

spannedBy :: Spanned a -> Spanned b -> c -> Spanned c
spannedBy a b p =
  At (span a <> span b) p

-- USE IN PARSERS

fromSourcePos :: Pos.SourcePos -> Position
fromSourcePos = liftA2 Position Pos.sourceLine Pos.sourceColumn

currentPosition :: Parser Position
currentPosition = fromSourcePos <$> getPosition

spanned :: Parser a -> Parser (Spanned a)
spanned p =
  flip . at <$> currentPosition <*> p <*> currentPosition


-- PRETTY PRINTING

instance Pretty Position where
  pretty (Position l c) =
    pretty l <> ":" <> pretty c

instance Pretty Span where
  pretty (Span f t) =
    pretty f <+> "to" <+> pretty t

instance Pretty a => Pretty (Spanned a) where
  pretty (At _ a) =
    pretty a -- <+> pretty ("@" :: Text) <+> pretty s
