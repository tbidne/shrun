{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Bench.Prelude
import Control.DeepSeq (force)
import Effectful.FileSystem.PathReader.Static (PathReaderStatic)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.FileSystem.Utils qualified as FsUtils
import Shrun.Prelude hiding (IO)
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.IO qualified as IO
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

fileLogs :: OsPath -> Benchmark
fileLogs testDir = bgroup "File Logging" (runLoops ["-f", FsUtils.unsafeDecodeOsToFp fp, "--no-config"])
  where
    fp = testDir </> [osp|bench.log|]

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

setup :: IO OsPath
setup = runEff' $ do
  testDir <-
    (\tmp -> tmp </> [osp|shrun|] </> [osp|bench|])
      <$> PR.getTemporaryDirectory
  PW.createDirectoryIfMissing True testDir
  pure testDir

teardown :: OsPath -> IO ()
teardown testDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = runEff' $ PW.removePathForcibly testDir
    doNothing =
      IO.putStrLn
        $ "*** Not cleaning up tmp dir: "
        <> FsUtils.decodeOsToFpShow testDir

runEff' :: Eff [PathWriterStatic, PathReaderStatic, IOE] a -> IO a
runEff' =
  runEff
    . PR.runPathReaderStaticIO
    . PW.runPathWriterStaticIO
