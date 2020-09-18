-- This module is responsible for information/error reporting, specifically showing lines in a pretty way.
module Report where

import qualified Data.Text as Text
import Data.Text (Text)

import qualified Tag

import Prettyprinter
import Prettyprinter.Render.Terminal

boxed :: String -> Doc ann
boxed content =
  vsep
  [ "┌" <> pretty (replicate (length content + 2) '─') <> "┐"
  , "│" <+> pretty content <+> "│"
  , "└" <> pretty (replicate (length content + 2) '─') <> "┘"
  , ""
  ]


--


addLineNumber :: Int -> Int -> Doc AnsiStyle -> Doc AnsiStyle
addLineNumber width n line =
  let
    number =
      show n

    lineNumber =
      annotate (color Blue) (pretty (replicate (width - length number - 2) ' ') <+> pretty number <+> "|")
  in
    lineNumber <+> line

paddingBreak :: Int -> Doc AnsiStyle
paddingBreak width =
  pretty (replicate width ' ') <> annotate (color Blue) "|"

squiggly :: Int -> Int -> Int -> Doc AnsiStyle
squiggly width s end =
  paddingBreak width <> pretty (replicate s ' ') <> annotate (color Red) (pretty (replicate (end - s) '^'))

squigglyElongated :: Int -> Int -> Int -> Bool -> Doc AnsiStyle
squigglyElongated width s end isElongated =
  if isElongated then
    squiggly width s end <> annotate (color Red) "..." <+> annotate (color Black) "<- continues on following line(s)"
  else
    squiggly width s end

-- Line printing utilities

-- Defaults empty text
line :: Int -> Text -> Text
line lineNumber text | lineNumber < 0 = ""
line lineNumber text =
  case drop (lineNumber - 1) (Text.lines text) of
    [] -> ""
    (x:_) -> x

prettyLineSquiggly :: Int -> Int -> Int -> Int -> Doc AnsiStyle -> Doc AnsiStyle
prettyLineSquiggly width lineNumber s end content =
  vsep
  [ paddingBreak width
  , addLineNumber width lineNumber content
  , squiggly width s end
  , paddingBreak width
  ]

prettyIndent :: Int -> Doc AnsiStyle -> Doc AnsiStyle
prettyIndent width content =
  vsep
  [ paddingBreak width
  , paddingBreak width <+> content
  , paddingBreak width
  , ""
  ]

prettyLine :: Int -> Int -> Doc AnsiStyle -> Doc AnsiStyle
prettyLine width lineNumber content =
  vsep
  [ paddingBreak width
  , addLineNumber width lineNumber content
  , paddingBreak width
  , ""
  ]

prettyLines :: Int -> Int -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyLines width lineNumber content =
  vsep $
    [ paddingBreak width ] <> fmap (addLineNumber width lineNumber) content <> [ paddingBreak width, "" ]
  
prettySpanSquiggly :: Int -> Tag.Span -> Text -> Doc AnsiStyle
prettySpanSquiggly width (Tag.Span (Tag.Position startLine startColumn) (Tag.Position endLine endColumn)) sourceCode =
  let
    ln line =
      addLineNumber width line (pretty (Report.line line sourceCode))
  in
    case startLine of
      1 ->
        vsep $
          [ paddingBreak width
          , ln startLine
          , squigglyElongated width startColumn (Text.length (Report.line startLine sourceCode) + 1) (startLine /= endLine)
          , paddingBreak width
          ]
      _ ->
        vsep $
          [ paddingBreak width
          , ln (startLine - 1)
          , ln startLine
          , squiggly width startColumn endColumn
          , paddingBreak width
          ]
