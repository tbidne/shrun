{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import ShellRun.Class.MonadShell qualified as MonadShell

main :: IO ()
main = MonadShell.runShell