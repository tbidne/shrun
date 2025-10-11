{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Tests for Shrun.Logging.Formatting.
module Unit.Shrun.Logging.Formatting (tests) where

import Data.Functor.Identity (Identity (Identity))
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (midday)
import Data.Time.LocalTime (utc)
import Effects.Time
  ( LocalTime (LocalTime),
    MonadTime (getMonotonicTime, getSystemZonedTime),
    ZonedTime (ZonedTime),
  )
import Shrun.Command.Types (CommandP (MkCommandP, command, index, key))
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch
      ( KeyHideOff,
        KeyHideOn
      ),
  )
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLogCmdSwitch (ConsoleLogCmdOff),
    ConsoleLoggingEnv,
    ConsoleLoggingP
      ( MkConsoleLoggingP,
        commandLogging,
        commandNameTrunc,
        lineTrunc,
        stripControl,
        timerFormat
      ),
  )
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat
  ( TimerFormat (ProseCompact),
  )
import Shrun.Configuration.Data.FileLogging
  ( DeleteOnSuccessSwitch (DeleteOnSuccessOff),
    FileLogOpened
      ( MkFileLogOpened,
        handle,
        queue
      ),
    FileLoggingEnv,
    FileLoggingP
      ( MkFileLoggingP,
        commandNameTrunc,
        deleteOnSuccess,
        file,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.StripControl
  ( StripControl
      ( StripControlAll,
        StripControlNone,
        StripControlSmart
      ),
  )
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelCommand, LevelFinished, LevelSuccess),
    LogMode (LogModeSet),
  )
import Unit.Generators qualified as PGens
import Unit.Prelude
import Unit.Shrun.Logging.Generators qualified as LGens

-- | Entry point for Shrun.Logging.Formatting property tests.
tests :: TestTree
tests =
  testGroup
    "Shrun.Logging.Formatting"
    [ consoleLogTests,
      fileLogTests,
      stripCharsTests
    ]

consoleLogTests :: TestTree
consoleLogTests =
  testGroup
    "formatConsoleLog"
    [ testFormatsCLNoCmd,
      testFormatsCLCmdKey,
      testFormatsCLCmdNoKey,
      testFormatsCLCommandNameTrunc,
      testFormatsCLLineTrunc,
      testFormatsCLSpecs
    ]

testFormatsCLNoCmd :: TestTree
testFormatsCLNoCmd = testPropertyNamed desc "testFormatsConsoleLogNoCmd" $ property $ do
  baseLog <- forAll LGens.genLogNoCmd

  -- keyHide should make no difference for NoCmd
  keyHide <- forAll LGens.genKeyHide

  let fmt = Formatting.formatConsoleLog keyHide baseConsoleLoggingEnv

  for_ (L.zip3 lvls prefixes suffixes) $ \(lvl, prefix, suffix) -> do
    let log = set' #lvl lvl baseLog
        -- NOTE: Need to perform the same stripping to match the result.
        expected =
          prefix
            <> T.stripStart (log ^. (#msg % #unLogMessage))
            <> suffix

        result = fmt log ^. #unConsoleLog
    expected === result
  where
    desc = "Formats with no command"
    lvls = [minBound .. maxBound]
    prefixes =
      [ "\ESC[97m[Debug] ",
        "\ESC[97m[Command] ",
        "\ESC[94m[Finished] ",
        "\ESC[96m[Timer] ",
        "\ESC[92m[Success] ",
        "\ESC[93m[Warn] ",
        "\ESC[91m[Error] ",
        "\ESC[91m[Fatal] ",
        "\ESC[91m[Killed] "
      ]
    suffixes = L.repeat "\ESC[0m"

testFormatsCLCmdKey :: TestTree
testFormatsCLCmdKey = testPropertyNamed desc "testFormatsCLCmdKey" $ property $ do
  baseLog <- forAll LGens.genLogNoCmd
  cmdKey <- forAll PGens.genText
  command <- forAll PGens.genText

  let cmd =
        MkCommandP
          { index = mkIdx 1,
            key = Just cmdKey,
            command
          }
      baseLog' = set' #cmd (Just cmd) baseLog

  let fmtKeyHideOff = Formatting.formatConsoleLog KeyHideOff baseConsoleLoggingEnv
      fmtKeyHideOn = Formatting.formatConsoleLog KeyHideOn baseConsoleLoggingEnv

  for_ (L.zip3 lvls prefixes suffixes) $ \(lvl, prefix, suffix) -> do
    let log = set' #lvl lvl baseLog'

        -- keys and commands __always__ have all control chars stripped

        expectedKeyHideOff =
          mconcat
            [ prefix,
              Formatting.formatCommandText cmdKey ^. #unUnlinedText,
              "] ",
              T.stripStart (log ^. #msg % #unLogMessage),
              suffix
            ]
        resultKeyHideOff = fmtKeyHideOff log ^. #unConsoleLog

        expectedKeyHideOn =
          mconcat
            [ prefix,
              Formatting.formatCommandText command ^. #unUnlinedText,
              "] ",
              T.stripStart (log ^. #msg % #unLogMessage),
              suffix
            ]
        resultKeyHideOn = fmtKeyHideOn log ^. #unConsoleLog

    expectedKeyHideOff === resultKeyHideOff
    expectedKeyHideOn === resultKeyHideOn
  where
    desc = "Formats with command and key"
    lvls = [minBound .. maxBound]
    prefixes =
      [ "\ESC[97m[Debug][",
        "\ESC[97m[Command][",
        "\ESC[94m[Finished][",
        "\ESC[96m[Timer][",
        "\ESC[92m[Success][",
        "\ESC[93m[Warn][",
        "\ESC[91m[Error][",
        "\ESC[91m[Fatal][",
        "\ESC[91m[Killed]["
      ]
    suffixes = L.repeat "\ESC[0m"

testFormatsCLCmdNoKey :: TestTree
testFormatsCLCmdNoKey = testPropertyNamed desc "testFormatsCLCmdNoKey" $ property $ do
  baseLog <- forAll LGens.genLogNoCmd
  command <- forAll PGens.genText

  -- keyHide should make no difference for no key
  keyHide <- forAll LGens.genKeyHide

  let cmd =
        MkCommandP
          { index = mkIdx 1,
            key = Nothing,
            command
          }
      baseLog' = set' #cmd (Just cmd) baseLog

  let fmt = Formatting.formatConsoleLog keyHide baseConsoleLoggingEnv

  for_ (L.zip3 lvls prefixes suffixes) $ \(lvl, prefix, suffix) -> do
    let log = set' #lvl lvl baseLog'

        expected =
          mconcat
            [ prefix,
              Formatting.formatCommandText command ^. #unUnlinedText,
              "] ",
              T.stripStart (log ^. #msg % #unLogMessage),
              suffix
            ]
        result = fmt log ^. #unConsoleLog

    expected === result
  where
    desc = "Formats with command but no key"
    lvls = [minBound .. maxBound]
    prefixes =
      [ "\ESC[97m[Debug][",
        "\ESC[97m[Command][",
        "\ESC[94m[Finished][",
        "\ESC[96m[Timer][",
        "\ESC[92m[Success][",
        "\ESC[93m[Warn][",
        "\ESC[91m[Error][",
        "\ESC[91m[Fatal][",
        "\ESC[91m[Killed]["
      ]
    suffixes = L.repeat "\ESC[0m"

testFormatsCLCommandNameTrunc :: TestTree
testFormatsCLCommandNameTrunc = testCase desc $ do
  "\ESC[92m[Success][some long key] msg len 10\ESC[0m" @=? fmt 13 baseLog ^. #unConsoleLog
  "\ESC[92m[Success][some long...] msg len 10\ESC[0m" @=? fmt 12 baseLog ^. #unConsoleLog
  "\ESC[92m[Success][...] msg len 10\ESC[0m" @=? fmt 0 baseLog ^. #unConsoleLog

  "\ESC[92m[Success][an even longer command] msg len 10\ESC[0m" @=? fmtKh 22 baseLog ^. #unConsoleLog
  "\ESC[92m[Success][an even longer com...] msg len 10\ESC[0m" @=? fmtKh 21 baseLog ^. #unConsoleLog
  "\ESC[92m[Success][...] msg len 10\ESC[0m" @=? fmtKh 0 baseLog ^. #unConsoleLog
  where
    baseLog =
      MkLog
        { cmd = Just (MkCommandP (mkIdx 1) (Just "some long key") "an even longer command"),
          lvl = LevelSuccess,
          msg = "msg len 10",
          mode = LogModeSet
        }
    desc = "Formats with cmd name truncation"
    -- key hide has no effect other than using the key over the cmd, which
    -- could have a different length, of course
    fmt n = Formatting.formatConsoleLog KeyHideOff (set' #commandNameTrunc (Just n) baseConsoleLoggingEnv)
    fmtKh n = Formatting.formatConsoleLog KeyHideOn (set' #commandNameTrunc (Just n) baseConsoleLoggingEnv)

testFormatsCLLineTrunc :: TestTree
testFormatsCLLineTrunc = testCase desc $ do
  -- 20 is just at the limit. Note that ASCII codes do not count
  "\ESC[92m[Success] msg len 10\ESC[0m" @=? fmt 20 baseLog ^. #unConsoleLog
  -- 19 is too low, we start to truncate
  "\ESC[92m[Success] msg le...\ESC[0m" @=? fmt 19 baseLog ^. #unConsoleLog
  -- we always print the prefix
  "\ESC[92m[Success] ...\ESC[0m" @=? fmt 0 baseLog ^. #unConsoleLog

  let logCmd =
        set'
          #cmd
          (Just $ MkCommandP (mkIdx 1) (Just "key") "some cmd")
          (set' #lvl LevelFinished baseLog)

  "\ESC[94m[Finished][key] msg len 10\ESC[0m" @=? fmt 26 logCmd ^. #unConsoleLog
  "\ESC[94m[Finished][key] msg le...\ESC[0m" @=? fmt 25 logCmd ^. #unConsoleLog
  "\ESC[94m[Finished][some cmd] msg len 10\ESC[0m" @=? fmtKh 31 logCmd ^. #unConsoleLog
  "\ESC[94m[Finished][some cmd] msg le...\ESC[0m" @=? fmtKh 30 logCmd ^. #unConsoleLog
  "\ESC[94m[Finished][some cmd] ...\ESC[0m" @=? fmtKh 0 logCmd ^. #unConsoleLog
  where
    baseLog =
      MkLog
        { cmd = Nothing,
          lvl = LevelSuccess,
          msg = "msg len 10",
          mode = LogModeSet
        }
    desc = "Formats with line truncation"
    -- key hide has no effect other than using the key over the cmd, which
    -- could have a different length, of course
    fmt n = Formatting.formatConsoleLog KeyHideOff (set' #lineTrunc (Just n) baseConsoleLoggingEnv)
    fmtKh n = Formatting.formatConsoleLog KeyHideOn (set' #lineTrunc (Just n) baseConsoleLoggingEnv)

testFormatsCLSpecs :: TestTree
testFormatsCLSpecs = testCase "Specific specs" $ do
  let l1 = MkLog (Just cmd1) "" LevelCommand LogModeSet
      cmd1 = MkCommandP (mkIdx 1) (Just "") "!\n!"

      expectedKeyHideOn1 = "\ESC[97m[Command][! !] \ESC[0m"
      resultKeyHideOn1 = fmtKeyHideOn l1 ^. #unConsoleLog
  expectedKeyHideOn1 @=? resultKeyHideOn1
  where
    fmtKeyHideOn = Formatting.formatConsoleLog KeyHideOn baseConsoleLoggingEnv

baseConsoleLoggingEnv :: ConsoleLoggingEnv
baseConsoleLoggingEnv =
  MkConsoleLoggingP
    { commandLogging = ConsoleLogCmdOff,
      commandNameTrunc = Nothing,
      lineTrunc = Nothing,
      stripControl = StripControlNone,
      timerFormat = ProseCompact
    }

fileLogTests :: TestTree
fileLogTests =
  testGroup
    "formatFileLog"
    [ testFormatsFLNoCmd,
      testFormatsFLCmdKey,
      testFormatsFLCmdNoKey,
      testFormatsFLCommandNameTrunc,
      testFormatsFLLineTrunc
    ]

testFormatsFLNoCmd :: TestTree
testFormatsFLNoCmd = testPropertyNamed desc "testFormatsFLNoCmd" $ property $ do
  baseLog <- forAll LGens.genLogNoCmd

  -- keyHide should make no difference for NoCmd
  keyHide <- forAll LGens.genKeyHide

  let fmt = runFormatFileLog keyHide baseFileLoggingEnv

  for_ (L.zip3 lvls prefixes suffixes) $ \(lvl, prefix, suffix) -> do
    let log = set' #lvl lvl baseLog
        expected = prefix <> (log ^. #msg % #unLogMessage) <> suffix
        result = fmt log
    expected === result
  where
    desc = "Formats with no command"
    lvls = [minBound .. maxBound]
    prefixes =
      (sysTimeNE <>)
        <$> [ "[Debug] ",
              "[Command] ",
              "[Finished] ",
              "[Timer] ",
              "[Success] ",
              "[Warn] ",
              "[Error] ",
              "[Fatal] ",
              "[Killed] "
            ]
    suffixes = L.repeat "\n"

testFormatsFLCmdKey :: TestTree
testFormatsFLCmdKey = testPropertyNamed desc "testFormatsFLCmdKey" $ property $ do
  baseLog <- forAll LGens.genLogNoCmd
  cmdKey <- forAll PGens.genText
  command <- forAll PGens.genText

  let cmd =
        MkCommandP
          { index = mkIdx 1,
            key = Just cmdKey,
            command
          }
      baseLog' = set' #cmd (Just cmd) baseLog

  let fmtKeyHideOff = runFormatFileLog KeyHideOff baseFileLoggingEnv
      fmtKeyHideOn = runFormatFileLog KeyHideOn baseFileLoggingEnv

  for_ (L.zip3 lvls prefixes suffixes) $ \(lvl, prefix, suffix) -> do
    let log = set' #lvl lvl baseLog'

        -- keys and commands __always__ have all control chars stripped
        -- Additionally, commands have whitespace stripped.

        expectedKeyHideOff =
          mconcat
            [ prefix,
              Formatting.formatCommandText cmdKey ^. #unUnlinedText,
              "] ",
              log ^. #msg % #unLogMessage,
              suffix
            ]
        resultKeyHideOff = fmtKeyHideOff log

        expectedKeyHideOn =
          mconcat
            [ prefix,
              Formatting.formatCommandText command ^. #unUnlinedText,
              "] ",
              log ^. #msg % #unLogMessage,
              suffix
            ]
        resultKeyHideOn = fmtKeyHideOn log

    expectedKeyHideOff === resultKeyHideOff
    expectedKeyHideOn === resultKeyHideOn
  where
    desc = "Formats with command and key"
    lvls = [minBound .. maxBound]
    prefixes =
      (sysTimeNE <>)
        <$> [ "[Debug][",
              "[Command][",
              "[Finished][",
              "[Timer][",
              "[Success][",
              "[Warn][",
              "[Error][",
              "[Fatal][",
              "[Killed]["
            ]
    suffixes = L.repeat "\n"

testFormatsFLCmdNoKey :: TestTree
testFormatsFLCmdNoKey = testPropertyNamed desc "testFormatsFLCmdNoKey" $ property $ do
  baseLog <- forAll LGens.genLogNoCmd
  command <- forAll PGens.genText

  -- keyHide should make no difference for no key
  keyHide <- forAll LGens.genKeyHide

  let cmd =
        MkCommandP
          { index = mkIdx 1,
            key = Nothing,
            command
          }
      baseLog' = set' #cmd (Just cmd) baseLog

  let fmt = runFormatFileLog keyHide baseFileLoggingEnv

  for_ (L.zip3 lvls prefixes suffixes) $ \(lvl, prefix, suffix) -> do
    let log = set' #lvl lvl baseLog'

        expected =
          mconcat
            [ prefix,
              Formatting.formatCommandText command ^. #unUnlinedText,
              "] ",
              log ^. #msg % #unLogMessage,
              suffix
            ]
        result = fmt log

    expected === result
  where
    desc = "Formats with command but no key"
    lvls = [minBound .. maxBound]
    prefixes =
      (sysTimeNE <>)
        <$> [ "[Debug][",
              "[Command][",
              "[Finished][",
              "[Timer][",
              "[Success][",
              "[Warn][",
              "[Error][",
              "[Fatal][",
              "[Killed]["
            ]
    suffixes = L.repeat "\n"

testFormatsFLCommandNameTrunc :: TestTree
testFormatsFLCommandNameTrunc = testCase desc $ do
  sysTimeNE <> "[Success][some long key] msg len 10\n" @=? fmt 13 baseLog
  sysTimeNE <> "[Success][some long...] msg len 10\n" @=? fmt 12 baseLog
  sysTimeNE <> "[Success][...] msg len 10\n" @=? fmt 0 baseLog

  sysTimeNE <> "[Success][an even longer command] msg len 10\n" @=? fmtKh 22 baseLog
  sysTimeNE <> "[Success][an even longer com...] msg len 10\n" @=? fmtKh 21 baseLog
  sysTimeNE <> "[Success][...] msg len 10\n" @=? fmtKh 0 baseLog
  where
    baseLog =
      MkLog
        { cmd = Just (MkCommandP (mkIdx 1) (Just "some long key") "an even longer command"),
          lvl = LevelSuccess,
          msg = "msg len 10",
          mode = LogModeSet
        }
    desc = "Formats with cmd name truncation"
    -- key hide has no effect other than using the key over the cmd, which
    -- could have a different length, of course
    fmt n = runFormatFileLog KeyHideOff (set' #commandNameTrunc (Just n) baseFileLoggingEnv)
    fmtKh n = runFormatFileLog KeyHideOn (set' #commandNameTrunc (Just n) baseFileLoggingEnv)

testFormatsFLLineTrunc :: TestTree
testFormatsFLLineTrunc = testCase desc $ do
  -- 20 is just at the limit. Note that the timestamp (21 chars) counts
  sysTimeNE <> "[Success] msg len 10\n" @=? fmt 41 baseLog
  -- 19 is too low, we start to truncate
  sysTimeNE <> "[Success] msg le...\n" @=? fmt 40 baseLog
  -- we always print the prefix
  sysTimeNE <> "[Success] ...\n" @=? fmt 21 baseLog

  let logCmd =
        set'
          #cmd
          (Just $ MkCommandP (mkIdx 1) (Just "key") "some cmd")
          (set' #lvl LevelFinished baseLog)

  sysTimeNE <> "[Finished][key] msg len 10\n" @=? fmt 47 logCmd
  sysTimeNE <> "[Finished][key] msg le...\n" @=? fmt 46 logCmd
  sysTimeNE <> "[Finished][some cmd] msg len 10\n" @=? fmtKh 52 logCmd
  sysTimeNE <> "[Finished][some cmd] msg le...\n" @=? fmtKh 51 logCmd
  sysTimeNE <> "[Finished][some cmd] ...\n" @=? fmtKh 0 logCmd
  where
    baseLog =
      MkLog
        { cmd = Nothing,
          lvl = LevelSuccess,
          msg = "msg len 10",
          mode = LogModeSet
        }
    desc = "Formats with line truncation"
    -- key hide has no effect other than using the key over the cmd, which
    -- could have a different length, of course
    fmt n = runFormatFileLog KeyHideOff (set' #lineTrunc (Just n) baseFileLoggingEnv)
    fmtKh n = runFormatFileLog KeyHideOn (set' #lineTrunc (Just n) baseFileLoggingEnv)

runFormatFileLog :: KeyHideSwitch -> FileLoggingEnv -> Log -> Text
runFormatFileLog keyHide env log =
  view #unFileLog
    $ Formatting.formatFileLog @MockTime keyHide env log
    ^. #runMockTime

-- The mock time our 'MonadTime' returns. Needs to be kept in sync with
-- getSystemZonedTime below.
sysTime :: (IsString a) => a
sysTime = "2020-05-31 12:00:00"

-- Bracketed sysTime
sysTimeNE :: Text
sysTimeNE = "[" <> sysTime <> "]"

newtype MockEnv = MkMockEnv ()

-- Monad with mock implementation for 'MonadTime'.
newtype MockTime a = MkMockTime
  { runMockTime :: a
  }
  deriving stock (Eq, Generic, Show)
  deriving (Applicative, Functor, Monad) via Identity

instance MonadTime MockTime where
  getSystemZonedTime = pure $ ZonedTime (LocalTime (toEnum 59_000) midday) utc
  getMonotonicTime = pure 0

instance MonadReader MockEnv MockTime where
  ask = pure $ MkMockEnv ()
  local _ = id

baseFileLoggingEnv :: FileLoggingEnv
baseFileLoggingEnv =
  MkFileLoggingP
    { file =
        MkFileLogOpened
          { handle = err "handle",
            queue = err "queue"
          },
      commandNameTrunc = Nothing,
      lineTrunc = Nothing,
      deleteOnSuccess = DeleteOnSuccessOff,
      stripControl = StripControlNone
    }
  where
    err f =
      error
        $ "[Unit.Props.Shrun.Logging.Queue]: Unit tests should not be using fileLogOpened."
        ++ f

stripCharsTests :: TestTree
stripCharsTests =
  testGroup
    "stripChars"
    [ stripNone,
      stripAll,
      stripSmart
    ]

stripNone :: TestTree
stripNone =
  testCase "StripControlNone should strip nothing" $ do
    "" @=? stripNone' ""
    -- NOTE: Previously this test said '"StripControlNone should
    -- "StripControlNone should strip nothing".
    --
    -- That was actually mistaken, as StripControlNone in fact did nothing.
    -- The only reason newlines were stripped was due to the underlying type
    -- being UnlineText, whose IsString impl stripped them. But this doesn't
    -- actually have anything to do w/ stripChars.
    --
    -- Now that the type is a LogMessage w/ a different IsString, we correctly
    -- show that newlines are preserved.
    " \n \ESC[ foo \ESC[A  bar \n baz \t  " @=? stripNone' " \n \ESC[ foo \ESC[A  bar \n baz \t  "
  where
    stripNone' = flip Formatting.stripChars StripControlNone

stripAll :: TestTree
stripAll =
  testCase "StripControlAll should strip all control" $ do
    "" @=? stripAll' ""
    "  oo    bar  baz   " @=? stripAll' " \n \ESC[ foo \ESC[A \ESC[K  bar \n baz \t  "
  where
    stripAll' = flip Formatting.stripChars StripControlAll

stripSmart :: TestTree
stripSmart =
  testCase "StripControlSmart should strip some control" $ do
    "" @=? stripSmart' ""
    "   foo \ESC[m   bar  baz   " @=? stripSmart' " \n \ESC[G foo \ESC[m \ESC[X  bar \n baz \t  "
  where
    stripSmart' = flip Formatting.stripChars StripControlSmart
