module Main where

import Lib

import Options.Applicative
import Config

main :: IO ()
main =
  execParser opts >>= solveAST
