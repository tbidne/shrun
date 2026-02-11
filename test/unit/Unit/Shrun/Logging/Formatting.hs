{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Tests for Shrun.Logging.Formatting.
module Unit.Shrun.Logging.Formatting (tests) where

import Data.HashMap.Strict qualified as Map
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (midday)
import Data.Time.LocalTime (utc)
import Effects.Time
  ( LocalTime (LocalTime),
    MonadTime (getMonotonicTime, getSystemZonedTime),
    ZonedTime (ZonedTime),
  )
import Shrun.Command.Types
  ( CommandIndex,
    CommandP (MkCommandP, command, index, key),
    CommandP1,
    CommandStatus (CommandFailure, CommandRunning, CommandSuccess, CommandWaiting),
  )
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch
      ( MkKeyHideSwitch
      ),
  )
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLogCmdSwitch (MkConsoleLogCmdSwitch),
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
  ( DeleteOnSuccessSwitch (MkDeleteOnSuccessSwitch),
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
import Shrun.Configuration.Env.Types (HasCommands (getCleanup, getCommandDepGraph, getCommandStatusMap))
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.Types
  ( ConsoleLog,
    Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelCommand, LevelError, LevelFinished, LevelSuccess),
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
      testFormatsCLSpecs,
      testFormatsCLMultiLine
    ]

testFormatsCLNoCmd :: TestTree
testFormatsCLNoCmd = testPropertyNamed desc "testFormatsConsoleLogNoCmd" $ property $ do
  baseLog <- forAll LGens.genLogNoCmd

  -- keyHide should make no difference for NoCmd
  keyHide <- forAll LGens.genKeyHide

  let fmt = runFmtConsole . Formatting.formatConsoleLog keyHide baseConsoleLoggingEnv

  for_ (L.zip3 lvls prefixes suffixes) $ \(lvl, prefix, suffix) -> do
    let log = set' #lvl lvl baseLog
        -- NOTE: Need to perform the same stripping to match the result.
        expected =
          prefix
            <> T.stripStart (log ^. (#msg % #unLogMessage))
            <> suffix

    result <- liftIO $ fmt log
    expected === result
  where
    desc = "Formats with no command"
    lvls = [minBound .. maxBound]
    prefixes =
      [ "\ESC[97m[Debug] ",
        "\ESC[97m[Command] ",
        "\ESC[94m[Finished][1|2|3|4] ",
        "\ESC[96m[Status][1|2|3|4] ",
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

  let fmtKeyHideOff = runFmtConsole . Formatting.formatConsoleLog (MkKeyHideSwitch False) baseConsoleLoggingEnv
      fmtKeyHideOn = runFmtConsole . Formatting.formatConsoleLog (MkKeyHideSwitch True) baseConsoleLoggingEnv

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
    resultKeyHideOff <- liftIO $ fmtKeyHideOff log

    let expectedKeyHideOn =
          mconcat
            [ prefix,
              Formatting.formatCommandText command ^. #unUnlinedText,
              "] ",
              T.stripStart (log ^. #msg % #unLogMessage),
              suffix
            ]
    resultKeyHideOn <- liftIO $ fmtKeyHideOn log

    expectedKeyHideOff === resultKeyHideOff
    expectedKeyHideOn === resultKeyHideOn
  where
    desc = "Formats with command and key"
    lvls = [minBound .. maxBound]
    prefixes =
      [ "\ESC[97m[Debug][",
        "\ESC[97m[Command][",
        "\ESC[94m[Finished][1|2|3|4][",
        "\ESC[96m[Status][1|2|3|4][",
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

  let fmt = runFmtConsole . Formatting.formatConsoleLog keyHide baseConsoleLoggingEnv

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
    result <- liftIO $ fmt log

    expected === result
  where
    desc = "Formats with command but no key"
    lvls = [minBound .. maxBound]
    prefixes =
      [ "\ESC[97m[Debug][",
        "\ESC[97m[Command][",
        "\ESC[94m[Finished][1|2|3|4][",
        "\ESC[96m[Status][1|2|3|4][",
        "\ESC[92m[Success][",
        "\ESC[93m[Warn][",
        "\ESC[91m[Error][",
        "\ESC[91m[Fatal][",
        "\ESC[91m[Killed]["
      ]
    suffixes = L.repeat "\ESC[0m"

testFormatsCLCommandNameTrunc :: TestTree
testFormatsCLCommandNameTrunc = testCase desc $ do
  ("\ESC[92m[Success][some long key] msg len 10\ESC[0m" @=?) =<< runFmtConsole (fmt 13 baseLog)
  ("\ESC[92m[Success][some long...] msg len 10\ESC[0m" @=?) =<< runFmtConsole (fmt 12 baseLog)
  ("\ESC[92m[Success][...] msg len 10\ESC[0m" @=?) =<< runFmtConsole (fmt 0 baseLog)

  ("\ESC[92m[Success][an even longer command] msg len 10\ESC[0m" @=?) =<< runFmtConsole (fmtKh 22 baseLog)
  ("\ESC[92m[Success][an even longer com...] msg len 10\ESC[0m" @=?) =<< runFmtConsole (fmtKh 21 baseLog)
  ("\ESC[92m[Success][...] msg len 10\ESC[0m" @=?) =<< runFmtConsole (fmtKh 0 baseLog)
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
    fmt n = Formatting.formatConsoleLog (MkKeyHideSwitch False) (set' #commandNameTrunc (Just n) baseConsoleLoggingEnv)
    fmtKh n = Formatting.formatConsoleLog (MkKeyHideSwitch True) (set' #commandNameTrunc (Just n) baseConsoleLoggingEnv)

testFormatsCLLineTrunc :: TestTree
testFormatsCLLineTrunc = testCase desc $ do
  -- 20 is just at the limit. Note that ASCII codes do not count
  ("\ESC[92m[Success] msg len 10\ESC[0m" @=?) =<< runFmtConsole (fmt 20 baseLog)
  -- 19 is too low, we start to truncate
  ("\ESC[92m[Success] msg le...\ESC[0m" @=?) =<< runFmtConsole (fmt 19 baseLog)
  -- we always print the prefix
  ("\ESC[92m[Success] ...\ESC[0m" @=?) =<< runFmtConsole (fmt 0 baseLog)

  let logCmd =
        set'
          #cmd
          (Just $ MkCommandP (mkIdx 1) (Just "key") "some cmd")
          (set' #lvl LevelFinished baseLog)

  ("\ESC[94m[Finished][1|2|3|4][key] msg len 10\ESC[0m" @=?) =<< runFmtConsole (fmt 35 logCmd)
  ("\ESC[94m[Finished][1|2|3|4][key] msg le...\ESC[0m" @=?) =<< runFmtConsole (fmt 34 logCmd)
  ("\ESC[94m[Finished][1|2|3|4][some cmd] msg len 10\ESC[0m" @=?) =<< runFmtConsole (fmtKh 40 logCmd)
  ("\ESC[94m[Finished][1|2|3|4][some cmd] msg le...\ESC[0m" @=?) =<< runFmtConsole (fmtKh 39 logCmd)
  ("\ESC[94m[Finished][1|2|3|4][some cmd] ...\ESC[0m" @=?) =<< runFmtConsole (fmtKh 0 logCmd)
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
    fmt n = Formatting.formatConsoleLog (MkKeyHideSwitch False) (set' #lineTrunc (Just n) baseConsoleLoggingEnv)
    fmtKh n = Formatting.formatConsoleLog (MkKeyHideSwitch True) (set' #lineTrunc (Just n) baseConsoleLoggingEnv)

testFormatsCLSpecs :: TestTree
testFormatsCLSpecs = testCase "Specific specs" $ do
  let l1 = MkLog (Just cmd1) "" LevelCommand LogModeSet
      cmd1 = MkCommandP (mkIdx 1) (Just "") "!\n!"

      expectedKeyHideOn1 = "\ESC[97m[Command][! !] \ESC[0m"
  resultKeyHideOn1 <- fmtKeyHideOn l1
  expectedKeyHideOn1 @=? resultKeyHideOn1
  where
    fmtKeyHideOn =
      runFmtConsole
        . Formatting.formatConsoleLog (MkKeyHideSwitch True) baseConsoleLoggingEnv

testFormatsCLMultiLine :: TestTree
testFormatsCLMultiLine = testCase "Formats multiline" $ do
  let l1 = MkLog (Just cmd1) "some error" LevelError LogModeSet
      l2 = MkLog (Just cmd1) "more output" LevelError LogModeSet
      cmd1 = MkCommandP (mkIdx 1) (Just "") "cmd"

  result <- fmt (l1 :| [l2])

  expected @=? result
  where
    fmt =
      runFmtConsole
        . Formatting.formatConsoleMultiLineLogs (MkKeyHideSwitch False) baseConsoleLoggingEnv

    expected = "\ESC[91m[Error][] some error\n  more output\ESC[0m"

runFmtConsole :: MockFormat ConsoleLog -> IO Text
runFmtConsole mf = do
  log <- runMockFormat mf
  pure $ log ^. #unConsoleLog

baseConsoleLoggingEnv :: ConsoleLoggingEnv
baseConsoleLoggingEnv =
  MkConsoleLoggingP
    { commandLogging = MkConsoleLogCmdSwitch False,
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
    result <- liftIO $ fmt log
    expected === result
  where
    desc = "Formats with no command"
    lvls = [minBound .. maxBound]
    prefixes =
      (sysTimeNE <>)
        <$> [ "[Debug] ",
              "[Command] ",
              "[Finished][1|2|3|4] ",
              "[Status][1|2|3|4] ",
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

  let fmtKeyHideOff = runFormatFileLog (MkKeyHideSwitch False) baseFileLoggingEnv
      fmtKeyHideOn = runFormatFileLog (MkKeyHideSwitch True) baseFileLoggingEnv

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
    resultKeyHideOff <- liftIO $ fmtKeyHideOff log

    let expectedKeyHideOn =
          mconcat
            [ prefix,
              Formatting.formatCommandText command ^. #unUnlinedText,
              "] ",
              log ^. #msg % #unLogMessage,
              suffix
            ]
    resultKeyHideOn <- liftIO $ fmtKeyHideOn log

    expectedKeyHideOff === resultKeyHideOff
    expectedKeyHideOn === resultKeyHideOn
  where
    desc = "Formats with command and key"
    lvls = [minBound .. maxBound]
    prefixes =
      (sysTimeNE <>)
        <$> [ "[Debug][",
              "[Command][",
              "[Finished][1|2|3|4][",
              "[Status][1|2|3|4][",
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
    result <- liftIO $ fmt log

    expected === result
  where
    desc = "Formats with command but no key"
    lvls = [minBound .. maxBound]
    prefixes =
      (sysTimeNE <>)
        <$> [ "[Debug][",
              "[Command][",
              "[Finished][1|2|3|4][",
              "[Status][1|2|3|4][",
              "[Success][",
              "[Warn][",
              "[Error][",
              "[Fatal][",
              "[Killed]["
            ]
    suffixes = L.repeat "\n"

testFormatsFLCommandNameTrunc :: TestTree
testFormatsFLCommandNameTrunc = testCase desc $ do
  (sysTimeNE <> "[Success][some long key] msg len 10\n" @=?) =<< fmt 13 baseLog
  (sysTimeNE <> "[Success][some long...] msg len 10\n" @=?) =<< fmt 12 baseLog
  (sysTimeNE <> "[Success][...] msg len 10\n" @=?) =<< fmt 0 baseLog

  (sysTimeNE <> "[Success][an even longer command] msg len 10\n" @=?) =<< fmtKh 22 baseLog
  (sysTimeNE <> "[Success][an even longer com...] msg len 10\n" @=?) =<< fmtKh 21 baseLog
  (sysTimeNE <> "[Success][...] msg len 10\n" @=?) =<< fmtKh 0 baseLog
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
    fmt n = runFormatFileLog (MkKeyHideSwitch False) (set' #commandNameTrunc (Just n) baseFileLoggingEnv)
    fmtKh n = runFormatFileLog (MkKeyHideSwitch True) (set' #commandNameTrunc (Just n) baseFileLoggingEnv)

testFormatsFLLineTrunc :: TestTree
testFormatsFLLineTrunc = testCase desc $ do
  -- 20 is just at the limit. Note that the timestamp (21 chars) counts
  (sysTimeNE <> "[Success] msg len 10\n" @=?) =<< fmt 41 baseLog
  -- 19 is too low, we start to truncate
  (sysTimeNE <> "[Success] msg le...\n" @=?) =<< fmt 40 baseLog
  -- we always print the prefix
  (sysTimeNE <> "[Success] ...\n" @=?) =<< fmt 21 baseLog

  let logCmd =
        set'
          #cmd
          (Just $ MkCommandP (mkIdx 1) (Just "key") "some cmd")
          (set' #lvl LevelFinished baseLog)

  (sysTimeNE <> "[Finished][1|2|3|4][key] msg len 10\n" @=?) =<< fmt 56 logCmd
  (sysTimeNE <> "[Finished][1|2|3|4][key] msg le...\n" @=?) =<< fmt 55 logCmd
  (sysTimeNE <> "[Finished][1|2|3|4][some cmd] msg len 10\n" @=?) =<< fmtKh 61 logCmd
  (sysTimeNE <> "[Finished][1|2|3|4][some cmd] msg le...\n" @=?) =<< fmtKh 60 logCmd
  (sysTimeNE <> "[Finished][1|2|3|4][some cmd] ...\n" @=?) =<< fmtKh 0 logCmd
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
    fmt n = runFormatFileLog (MkKeyHideSwitch False) (set' #lineTrunc (Just n) baseFileLoggingEnv)
    fmtKh n = runFormatFileLog (MkKeyHideSwitch True) (set' #lineTrunc (Just n) baseFileLoggingEnv)

runFormatFileLog :: KeyHideSwitch -> FileLoggingEnv -> Log -> IO Text
runFormatFileLog keyHide env log = do
  flog <- runMockTime $ Formatting.formatFileLog @_ @MockTime keyHide env log
  pure $ flog ^. #unFileLog

-- The mock time our 'MonadTime' returns. Needs to be kept in sync with
-- getSystemZonedTime below.
sysTime :: (IsString a) => a
sysTime = "2020-05-31 12:00:00"

-- Bracketed sysTime
sysTimeNE :: Text
sysTimeNE = "[" <> sysTime <> "]"

newtype MockEnv = MkMockEnv
  { commandStatusMap :: HashMap CommandIndex (CommandP1, TVar CommandStatus)
  }
  deriving stock (Generic)

mkMockEnv :: IO MockEnv
mkMockEnv = do
  tvars <- atomically $ for statuses $ \(i, s) -> do
    let idx = toEnum i
        cmd = MkCommandP idx Nothing ("cmd" <> showt i)
    ts <- newTVar' s
    pure (idx, (cmd, ts))
  let commandStatusMap = Map.fromList tvars

  pure
    $ MkMockEnv
      { commandStatusMap
      }
  where
    statuses =
      zip
        [1 ..]
        [ CommandWaiting,
          CommandRunning (Nothing, []),
          CommandRunning (Nothing, []),
          CommandFailure,
          CommandFailure,
          CommandFailure,
          CommandSuccess,
          CommandSuccess,
          CommandSuccess,
          CommandSuccess
        ]

instance HasCommands MockEnv where
  getCleanup _ = Nothing

  getCommandDepGraph _ = error "todo"

  getCommandStatusMap = view #commandStatusMap

-- Monad with mock implementation for 'MonadTime'.
newtype MockTime a = MkMockTime (ReaderT MockEnv IO a)
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadAtomic,
      MonadReader MockEnv
    )

runMockTime :: MockTime a -> IO a
runMockTime (MkMockTime io) = do
  env <- mkMockEnv
  runReaderT io env

instance MonadTime MockTime where
  getSystemZonedTime = pure $ ZonedTime (LocalTime (toEnum 59_000) midday) utc
  getMonotonicTime = pure 0

newtype MockFormat a = MkMockFormat (ReaderT MockEnv IO a)
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadAtomic,
      MonadReader MockEnv
    )

runMockFormat :: MockFormat a -> IO a
runMockFormat (MkMockFormat io) = do
  env <- mkMockEnv
  runReaderT io env

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
      deleteOnSuccess = MkDeleteOnSuccessSwitch False,
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
