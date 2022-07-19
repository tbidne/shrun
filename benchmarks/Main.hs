module Main (main) where

import Control.DeepSeq (force)
import Shrun (runShell, runShellT)
import Shrun.Configuration.Env (makeEnv)
import Shrun.Prelude
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
noLogs = bgroup "No Logging" (runLoops ["-d", "--no-config"])

basicLogs :: Benchmark
basicLogs = bgroup "Basic Logging" (runLoops ["--no-config"])

cmdLogs :: Benchmark
cmdLogs = bgroup "Command Logging" (runLoops ["-l", "--no-config"])

runLoops :: [String] -> [Benchmark]
runLoops args = fmap f loops
  where
    f (!desc, !cmd) =
      run desc (cmd : args)

run :: String -> [String] -> Benchmark
run desc args =
  bench desc $ nfIO $ do
    srEnv <- withArgs args makeEnv
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
