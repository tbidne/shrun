module Main (main) where

import Benchmark.Prelude
import Control.DeepSeq (NFData)
import Criterion qualified
import Criterion.Main qualified
import Data.Word (Word32)
import ShellRun qualified as SR
import ShellRun.Env qualified as Env
import System.Environment qualified as SysEnv

data Step
  = Linear {-# UNPACK #-} !Word32
  | Exponential {-# UNPACK #-} !Word32
  deriving (Eq, Show)

data Range = MkRange
  { start :: {-# UNPACK #-} !Word32,
    end :: {-# UNPACK #-} !Word32,
    step :: {-# UNPACK #-} !Step
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  Criterion.Main.defaultMain
    [ scaleCmdTime,
      scaleCmdTimeWithLogging,
      scaleNumCmds
    ]

scaleCmdTime :: Benchmark
scaleCmdTime =
  Criterion.bgroup
    "Scale command time"
    (replicateBench range "" id loopCmd)
  where
    range = MkRange {start = 1, end = 10, step = Linear 1}

scaleCmdTimeWithLogging :: Benchmark
scaleCmdTimeWithLogging =
  Criterion.bgroup
    "Scale command time with logging"
    (replicateBench range "" id withLogging)
  where
    range = MkRange {start = 1, end = 10, step = Linear 1}
    withLogging = loopCmdWithArgs ["--cmd-log"]

loopCmd :: Word32 -> IO ()
loopCmd = loopCmdWithArgs []

loopCmdWithArgs :: List (List Char) -> Word32 -> IO ()
loopCmdWithArgs extraArgs n = do
  let argList = "-d" : extraArgs <> [cmd]
  env <- SysEnv.withArgs argList Env.runParser
  SR.runShellT SR.runShell env
  where
    cmd = "for i in {1.." <> show n <> "}; do echo hi; done;"

scaleNumCmds :: Benchmark
scaleNumCmds =
  Criterion.bgroup
    "Scale number of commands"
    (replicateBench range "" intToCmd nCmds)
  where
    range = MkRange {start = 1, end = 100, step = Linear 10}

    intToCmd :: Word32 -> List (List Char)
    intToCmd i = replicate (w32ToInt i) "sleep 0"

    nCmds :: List (List Char) -> IO ()
    nCmds cmds = do
      let argList = "-d" : cmds
      env <- SysEnv.withArgs argList Env.runParser
      SR.runShellT SR.runShell env

replicateBench ::
  forall a b.
  NFData a =>
  Range ->
  List Char ->
  (Word32 -> a) ->
  (a -> IO b) ->
  List Benchmark
replicateBench range baseLabel inputFn ioFn = fmap singleCmd inputsWithIndex
  where
    inputsWithIndex :: List (Tuple2 Word32 a)
    inputsWithIndex = fmap (\i -> (i, inputFn i)) (rangeToList range)

    singleCmd :: (Word32, a) -> Benchmark
    singleCmd (idx, n) =
      Criterion.env
        (pure n)
        (Criterion.bench (baseLabel <> show idx) . Criterion.whnfIO . ioFn)

rangeToList :: Range -> [Word32]
rangeToList MkRange {start, end, step = (Linear s)} = [start, start + s .. end]
rangeToList MkRange {start, end, step = (Exponential s)} = go start
  where
    go t
      | t > end = []
      | otherwise = t : go t'
      where
        !t' = t * s

w32ToInt :: Word32 -> Int
w32ToInt = fromIntegral
