module Main (main) where

import Bench.Prelude
import Control.DeepSeq (force)
import Effects.FileSystem.PathReader qualified as RDir
import Effects.FileSystem.PathWriter qualified as WDir
import Shrun.Prelude hiding (IO)
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nfIO,
  )
import Prelude (IO)

main :: IO ()
main = bracket setup teardown runBenchmarks
  where
    runBenchmarks testDir =
      defaultMain
        [ basicLogs,
          cmdLogs,
          fileLogs testDir
        ]

basicLogs :: Benchmark
basicLogs = bgroup "Basic Logging" (runLoops ["--no-config"])

cmdLogs :: Benchmark
cmdLogs = bgroup "Command Logging" (runLoops ["-l", "--no-config"])

fileLogs :: FilePath -> Benchmark
fileLogs testDir = bgroup "File Logging" (runLoops ["-f", fp, "--no-config"])
  where
    fp = testDir </> "bench.log"

runLoops :: List String -> List Benchmark
runLoops args = fmap f loops
  where
    f (!desc, !cmd) =
      run desc (cmd : args)

run :: String -> List String -> Benchmark
run desc = bench desc . nfIO . runBench

loops :: List (String, String)
loops =
  force
    [ ("10_000", bashLoop "10000"),
      ("100_000", bashLoop "100000"),
      ("1_000_000", bashLoop "1000000")
    ]

bashLoop :: String -> String
bashLoop bound = "for i in {1.." ++ bound ++ "}; do echo ${i}; done"

setup :: IO FilePath
setup = do
  testDir <- (\tmp -> tmp </> "shrun" </> "bench") <$> RDir.getTemporaryDirectory
  WDir.createDirectoryIfMissing True testDir
  pure testDir

teardown :: FilePath -> IO ()
teardown testDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = WDir.removePathForcibly testDir
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> testDir
