-- | Provides various generators for property tests.
module Unit.Shrun.Logging.Generators
  ( -- * Log
    genLog,
    genLogWithCmd,
    genLogNoCmd,
    genLogWithCmdKey,

    -- * Helpers
    genLogLevel,
    genLogMode,
  )
where

import Hedgehog.Gen qualified as HGen
import Shrun.Data.Command (Command (MkCommand), CommandP1)
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel,
    LogMode,
  )
import Unit.Generators qualified as PGens
import Unit.Prelude

genLog :: Gen Log
genLog = do
  cmd <- HGen.choice [pure Nothing, fmap Just genCommand]
  msg <- PGens.genText
  lvl <- genLogLevel
  mode <- genLogMode
  pure
    $ MkLog
      { cmd,
        msg,
        lvl,
        mode
      }

genLogWithCmd :: Gen Log
genLogWithCmd = do
  cmd <- Just <$> genCommand
  msg <- PGens.genText
  lvl <- genLogLevel
  mode <- genLogMode
  pure
    $ MkLog
      { cmd,
        msg,
        lvl,
        mode
      }

genLogWithCmdKey :: Gen Log
genLogWithCmdKey = do
  cmd <- Just <$> genCommandWithKey
  msg <- PGens.genText
  lvl <- genLogLevel
  mode <- genLogMode
  pure
    $ MkLog
      { cmd,
        msg,
        lvl,
        mode
      }

genLogNoCmd :: Gen Log
genLogNoCmd = do
  msg <- PGens.genText
  lvl <- genLogLevel
  mode <- genLogMode
  pure
    $ MkLog
      { cmd = Nothing,
        msg,
        lvl,
        mode
      }

genLogLevel :: Gen LogLevel
genLogLevel = HGen.enumBounded

genLogMode :: Gen LogMode
genLogMode = HGen.enumBounded

genCommand :: Gen CommandP1
genCommand = HGen.choice [genCommandWithKey, genCommandNoKey]

genCommandWithKey :: Gen CommandP1
genCommandWithKey = MkCommand <$> fmap Just PGens.genText <*> PGens.genText

genCommandNoKey :: Gen CommandP1
genCommandNoKey = MkCommand Nothing <$> PGens.genText
