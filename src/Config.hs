module Config where

import qualified Data.Text as Text
import Data.Text (Text)

import Options.Applicative
import Data.Semigroup ((<>))

data Config =
  Config
  { showSteps :: Bool
  , showColours :: Bool
  , expression :: Text
  }
  deriving (Show, Eq)

data Source
  = Argument Text
  | Filepath FilePath
  deriving (Show, Eq)

config :: Parser Config
config =
  Config
  <$> switch
      ( long "steps"
     <> short 's'
     <> help "Show the steps of reduction" )
  <*> switch
      ( long "colours"
     <> short 'c'
     <> help "Show colours in steps"
     <> showDefault
      )
  <*> argument str (metavar "EXPRESSION")

opts :: ParserInfo Config
opts = info (config <**> helper)
  ( fullDesc
  <> progDesc "Solve polynomials up to 2nd degree"
  <> header "computor-v1"
  )
