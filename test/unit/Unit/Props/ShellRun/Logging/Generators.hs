-- | Provides various generators for property tests.
module Unit.Props.ShellRun.Logging.Generators
  ( -- * Log
    genLog,
    genLogWithCmd,
    genLogNoCmd,
    genLogWithCmdKey,

    -- * Helpers
    genLogLevel,
    genLogMode,
    genLogDest,
  )
where

import Hedgehog.Gen qualified as HGen
import ShellRun.Data.Command (Command (..))
import ShellRun.Logging.Types (Log (..), LogDest (..), LogLevel (..), LogMode (..))
import Unit.Prelude
import Unit.Props.Generators qualified as PGens

genLog :: Gen Log
genLog = do
  cmd' <- HGen.choice [pure Nothing, fmap Just genCommand]
  msg' <- PGens.genText
  lvl' <- genLogLevel
  mode' <- genLogMode
  dest' <- genLogDest
  pure $
    MkLog
      { cmd = cmd',
        msg = msg',
        lvl = lvl',
        mode = mode',
        dest = dest'
      }

genLogWithCmd :: Gen Log
genLogWithCmd = do
  cmd' <- Just <$> genCommand
  msg' <- PGens.genText
  lvl' <- genLogLevel
  mode' <- genLogMode
  dest' <- genLogDest
  pure $
    MkLog
      { cmd = cmd',
        msg = msg',
        lvl = lvl',
        mode = mode',
        dest = dest'
      }

genLogWithCmdKey :: Gen Log
genLogWithCmdKey = do
  cmd' <- Just <$> genCommandWithKey
  msg' <- PGens.genText
  lvl' <- genLogLevel
  mode' <- genLogMode
  dest' <- genLogDest
  pure $
    MkLog
      { cmd = cmd',
        msg = msg',
        lvl = lvl',
        mode = mode',
        dest = dest'
      }

genLogNoCmd :: Gen Log
genLogNoCmd = do
  msg' <- PGens.genText
  lvl' <- genLogLevel
  mode' <- genLogMode
  dest' <- genLogDest
  pure $
    MkLog
      { cmd = Nothing,
        msg = msg',
        lvl = lvl',
        mode = mode',
        dest = dest'
      }

genLogLevel :: Gen LogLevel
genLogLevel = HGen.enumBounded

genLogMode :: Gen LogMode
genLogMode = HGen.enumBounded

genLogDest :: Gen LogDest
genLogDest = HGen.enumBounded

genCommand :: Gen Command
genCommand = HGen.choice [genCommandWithKey, genCommandNoKey]

genCommandWithKey :: Gen Command
genCommandWithKey = MkCommand <$> fmap Just PGens.genText <*> PGens.genText

genCommandNoKey :: Gen Command
genCommandNoKey = MkCommand Nothing <$> PGens.genText
