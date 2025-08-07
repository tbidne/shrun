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

    -- * Misc
    genKeyHide,
  )
where

import Hedgehog.Gen qualified as HGen
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch (KeyHideSwitch)
import Shrun.Data.Command (CommandP (MkCommandP), CommandP1)
import Shrun.Data.Text (UnlinedText (UnsafeUnlinedText))
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel,
    LogMessage (UnsafeLogMessage),
    LogMode,
  )
import Unit.Generators qualified as PGens
import Unit.Prelude

genLog :: Gen Log
genLog = do
  cmd <- HGen.choice [pure Nothing, fmap Just genCommand]
  msg <- PGens.genUnlinedText
  lvl <- genLogLevel
  mode <- genLogMode
  pure
    $ MkLog
      { cmd,
        msg = coerce msg,
        lvl,
        mode
      }

genLogWithCmd :: Gen Log
genLogWithCmd = do
  cmd <- Just <$> genCommand
  msg <- PGens.genUnlinedText
  lvl <- genLogLevel
  mode <- genLogMode
  pure
    $ MkLog
      { cmd,
        msg = coerce msg,
        lvl,
        mode
      }

genLogWithCmdKey :: Gen Log
genLogWithCmdKey = do
  cmd <- Just <$> genCommandWithKey
  msg <- PGens.genUnlinedText
  lvl <- genLogLevel
  mode <- genLogMode
  pure
    $ MkLog
      { cmd,
        msg = coerce msg,
        lvl,
        mode
      }

genLogNoCmd :: Gen Log
genLogNoCmd = do
  msg <- PGens.genUnlinedText
  lvl <- genLogLevel
  mode <- genLogMode
  pure
    $ MkLog
      { cmd = Nothing,
        msg = coerce msg,
        lvl,
        mode
      }

genKeyHide :: Gen KeyHideSwitch
genKeyHide = HGen.enumBounded

genLogLevel :: Gen LogLevel
genLogLevel = HGen.enumBounded

genLogMode :: Gen LogMode
genLogMode = HGen.enumBounded

genCommand :: Gen CommandP1
genCommand = HGen.choice [genCommandWithKey, genCommandNoKey]

genCommandWithKey :: Gen CommandP1
genCommandWithKey = MkCommandP <$> fmap Just PGens.genText <*> PGens.genText

genCommandNoKey :: Gen CommandP1
genCommandNoKey = MkCommandP Nothing <$> PGens.genText
