{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Monad.Reader qualified as MTL
import ShellRun qualified as SR
import ShellRun.Parsing.Env qualified as Env

main :: IO ()
main = do
  env <- Env.runParser
  MTL.runReaderT (SR.runShellT SR.runShell) env
