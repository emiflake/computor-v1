{-# LANGUAGE OverloadedStrings #-}
module Util where

import Data.Text (Text)
import Prettyprinter.Render.Terminal
import qualified Tag
import qualified Expr
import Expr (Expr', Expr)
import Prettyprinter
import qualified Reduce
  
-- Utility function for GHCi

reduceAST :: Text -> IO ()
reduceAST text =
  case Expr.runParserE Expr.expr text of
    Right ast ->
      case Reduce.reduce1 ast of
        Left e ->
          putDoc $ Reduce.showError ast e
        Right v ->
          case v of
            Tag.At _ (Expr.LitNum d) ->
              putDoc $ "Fully reduced to" <+> annotate (color Green) (pretty d) <> hardline
            _ -> putDoc $ "Could not fully reduce, reduced to" <+> annotate (color Blue) (pretty v) <> hardline
    Left e ->
      print e

