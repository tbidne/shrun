{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import ShellRun qualified

main :: IO ()
main = ShellRun.runShell