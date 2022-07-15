module Main (main) where

import Control.DeepSeq (force)
import ShellRun (runShell, runShellT)
import ShellRun.Env (runParser)
import ShellRun.Prelude
import System.Environment (withArgs)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nfIO,
  )

main :: IO ()
main = do
  defaultMain
    [ noLogs,
      basicLogs,
      cmdLogs
    ]

noLogs :: Benchmark
noLogs = bgroup "No Logging" (runLoops ["-d"])

basicLogs :: Benchmark
basicLogs = bgroup "Basic Logging" (runLoops [])

cmdLogs :: Benchmark
cmdLogs = bgroup "Command Logging" (runLoops ["-c"])

runLoops :: [String] -> [Benchmark]
runLoops args = fmap f loops
  where
    f (!desc, !cmd) =
      run desc (cmd : args)

run :: String -> [String] -> Benchmark
run desc args =
  bench desc $ nfIO $ do
    srEnv <- withArgs args runParser
    runShellT runShell srEnv

loops :: [(String, String)]
loops =
  force
    [ ("10_000", bashLoop "10000"),
      ("100_000", bashLoop "100000"),
      ("1_000_000", bashLoop "1000000")
    ]

bashLoop :: String -> String
bashLoop bound = "for i in {1.." ++ bound ++ "}; do echo ${i}; done"
