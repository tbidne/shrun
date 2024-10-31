{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Bench.Prelude
import Control.DeepSeq (force)
import Effects.FileSystem.PathReader qualified as RDir
import Effects.FileSystem.PathWriter qualified as WDir
import FileSystem.OsPath (unsafeDecode)
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
          commandLogs,
          fileLogs testDir
        ]

basicLogs :: Benchmark
basicLogs = bgroup "Basic Logging" (runLoops ["--no-config"])

commandLogs :: Benchmark
commandLogs = bgroup "Command Logging" (runLoops ["--console-log-command", "--no-config"])

fileLogs :: OsPath -> Benchmark
fileLogs testDir = bgroup "File Logging" (runLoops ["-f", unsafeDecode fp, "--no-config"])
  where
    fp = testDir </> [osp|bench.log|]

runLoops :: List String -> List Benchmark
runLoops args = fmap f loops
  where
    f (!desc, !command) =
      run desc (command : args)

run :: String -> List String -> Benchmark
run desc = bench desc . nfIO . runBench

loops :: List (String, String)
loops =
  force
    [ ("10_000", bashLoop "10000"),
      ("100_000", bashLoop "100000")
    ]

-- We have trouble with CI not interpolating brace syntax correctly, but
-- apparently it works here? At least the benchmark results correctly show the
-- running time getting slower across the 3 params.
--
-- See NOTE: [Bash brace loop interpolation].
bashLoop :: String -> String
bashLoop bound = "for i in {1.." ++ bound ++ "}; do echo ${i}; done"

setup :: IO OsPath
setup = do
  testDir <-
    (\tmp -> tmp </> [osp|shrun|] </> [osp|bench|])
      <$> RDir.getTemporaryDirectory
  WDir.createDirectoryIfMissing True testDir
  pure testDir

teardown :: OsPath -> IO ()
teardown testDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = WDir.removePathForcibly testDir
    doNothing =
      putStrLn
        $ "*** Not cleaning up tmp dir: '"
        <> decodeLenient testDir
        <> "'"
