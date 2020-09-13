{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Tag where

import Prelude hiding (span)

import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Pos as Pos
import Data.Text (Text)
import qualified Data.Text as Text

import Control.Applicative

import Control.Lens hiding (at)

import Prettyprinter hiding (line)

-- TAGS

data Position
  = Position
  { _line :: Int
  , _column :: Int
  }
  deriving (Eq, Ord)

instance Show Position where
  show (Position l c) = "[" <> show l <> ":" <> show c <> "]"

makeLenses ''Position

data Span =
  Span Position Position
  deriving (Show, Ord, Eq)

data Spanned a
  = At Span a
  deriving (Show, Eq, Functor)

instance Ord a => Ord (Spanned a) where
  compare (At _ a) (At _ b) =
    compare a b
    

span :: Lens' (Spanned a) Span
span = lens (\(At s _) -> s) (\(At _ v) s -> At s v)

unspan :: Spanned a -> Span
unspan = view span

at :: Position -> Position -> a -> Spanned a
at f t =
  At (Span f t)

inLine :: Int -> Position -> Bool
inLine l = (==) l . view line

fullyInLine :: Int -> Span -> Bool
fullyInLine l (Span f t) = inLine l f && inLine l t

positionIn :: Position -> Span -> Bool
positionIn p (Span f t) =
  f <= p && p <= t

spanIn :: Span -> Span -> Bool
spanIn (Span f t) s =
  positionIn f s && positionIn t s

mergeSpans :: Span -> Span -> Span
mergeSpans (Span start _) (Span _ end) =
  Span start end

mergeSpanned :: Spanned a -> Spanned b -> Spanned (a, b)
mergeSpanned (At s a) (At s' b) = At (mergeSpans s s') (a, b)

spannedBy :: Spanned a -> Spanned b -> c -> Spanned c
spannedBy (view span -> a) (view span -> b) p =
  At (mergeSpans a b) p

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
