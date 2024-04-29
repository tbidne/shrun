{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests for Shrun.Logging.Formatting.
module Unit.Shrun.Logging.Formatting (tests) where

import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString)
import Data.Text qualified as T
import Data.Time (midday)
import Data.Time.LocalTime (utc)
import Effects.Time
  ( LocalTime (LocalTime),
    MonadTime (getMonotonicTime, getSystemZonedTime),
    ZonedTime (ZonedTime),
  )
import Hedgehog.Gen qualified as HGen
import Hedgehog.Internal.Range qualified as HRange
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLoggingP
      ( MkConsoleLoggingP,
        cmdLogging,
        cmdNameTrunc,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.FileLogging
  ( FileLogOpened
      ( MkFileLogOpened,
        handle,
        queue
      ),
    FileLoggingEnv,
    FileLoggingP (MkFileLoggingP, cmdNameTrunc, file, stripControl),
  )
import Shrun.Data.Command (CommandP (MkCommandP))
import Shrun.Data.KeyHide (KeyHide (KeyHideOff, KeyHideOn))
import Shrun.Data.StripControl
  ( StripControl
      ( StripControlAll,
        StripControlNone,
        StripControlSmart
      ),
  )
import Shrun.Data.Truncation
  ( TruncRegion (TCmdLine, TCmdName),
    Truncation (MkTruncation),
  )
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelCommand),
    LogMode (LogModeSet),
  )
import Shrun.Utils qualified as Utils
import Test.Tasty qualified as T
import Unit.Prelude
import Unit.Shrun.Logging.Generators qualified as LGens

data Env = MkEnv
  { keyHide :: KeyHide,
    cmdTrunc :: Maybe (Truncation TCmdName),
    lineTrunc :: Maybe (Truncation TCmdLine)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Env

genEnv :: Gen Env
genEnv =
  MkEnv
    <$> HGen.enumBounded
    <*> (fmap . fmap) MkTruncation genMInt
    <*> (fmap . fmap) MkTruncation genMInt

cmdTruncLimit :: (Integral a) => a
cmdTruncLimit = 30

genEnvCmdTrunc :: Gen (Env, Truncation TCmdName)
genEnvCmdTrunc =
  (,)
    <$> ( MkEnv
            <$> HGen.enumBounded
            <*> pure Nothing
            <*> pure Nothing
        )
    <*> fmap MkTruncation genNat
  where
    genNat = HGen.integral range
    range = HRange.linear 0 cmdTruncLimit

genLongCmdText :: Gen Text
genLongCmdText = HGen.text range HGen.unicode
  where
    range = HRange.linearFrom (cmdTruncLimit + 1) (cmdTruncLimit + 1) 100

lineTruncLimit :: (Integral a) => a
lineTruncLimit = 80

genEnvLineTrunc :: Gen Env
genEnvLineTrunc =
  MkEnv
    <$> HGen.enumBounded
    <*> pure Nothing
    <*> fmap (Just . MkTruncation) genNat
  where
    genNat = HGen.integral range
    range = HRange.linear 0 lineTruncLimit

genLongLineText :: Gen Text
genLongLineText = HGen.text range HGen.unicode
  where
    range = HRange.linearFrom (lineTruncLimit + 1) (lineTruncLimit + 1) 120

genEnvDispCmd :: Gen Env
genEnvDispCmd =
  MkEnv KeyHideOn
    <$> (fmap . fmap) MkTruncation genMInt
    <*> (fmap . fmap) MkTruncation genMInt

genEnvDispKey :: Gen Env
genEnvDispKey =
  MkEnv KeyHideOff
    <$> (fmap . fmap) MkTruncation genMInt
    <*> (fmap . fmap) MkTruncation genMInt

genMInt :: Gen (Maybe Natural)
genMInt = HGen.choice [Just <$> genNat, pure Nothing]
  where
    genNat = HGen.integral range
    range = HRange.exponential 0 100

-- | Entry point for Shrun.Logging.Formatting property tests.
tests :: TestTree
tests =
  T.testGroup
    "Shrun.Logging.Formatting"
    [ consoleLogProps,
      fileLogProps,
      stripCharsSpecs
    ]

consoleLogProps :: TestTree
consoleLogProps =
  T.testGroup
    "Console logs"
    [ messageProps,
      testFormatConsoleNullNoTrunc,
      testFormatConsoleNullTrunc0,
      prefixProps,
      displayCmdProps,
      displayKeyProps,
      cmdTruncProps,
      lineTruncProps
    ]

messageProps :: TestTree
messageProps =
  testPropertyNamed "Includes message" "messageProps"
    $ property
    $ do
      env <- forAll genEnv
      log@MkLog {msg} <- forAll LGens.genLog
      let result = formatConsoleLog env log
      annotate (T.unpack result)
      includesOrTruncated (T.strip msg) result

testFormatConsoleNullNoTrunc :: TestTree
testFormatConsoleNullNoTrunc = testCase "Null byte with no truncation" $ do
  let result = formatConsoleLog env log

  "\ESC[97m[Command] \NUL\ESC[0m" @=? result
  where
    env =
      MkEnv
        { keyHide = KeyHideOff,
          cmdTrunc = Nothing,
          lineTrunc = Nothing
        }
    log =
      MkLog
        { cmd = Nothing,
          msg = "\NUL",
          lvl = LevelCommand,
          mode = LogModeSet
        }

testFormatConsoleNullTrunc0 :: TestTree
testFormatConsoleNullTrunc0 = testCase "Null byte with truncation 0" $ do
  let result = formatConsoleLog env log

  "\ESC[97m[Command] \NUL\ESC[0m" @=? result
  where
    env =
      MkEnv
        { keyHide = KeyHideOff,
          cmdTrunc = Just 0,
          lineTrunc = Just 0
        }
    log =
      MkLog
        { cmd = Nothing,
          msg = "\NUL",
          lvl = LevelCommand,
          mode = LogModeSet
        }

prefixProps :: TestTree
prefixProps =
  testPropertyNamed "Formats prefix" "prefixProps"
    $ property
    $ do
      env <- forAll genEnv
      log@MkLog {lvl} <- forAll LGens.genLog
      let result = formatConsoleLog env log
      let pfx = Formatting.levelToPrefix lvl
      includesOrTruncated pfx result

displayCmdProps :: TestTree
displayCmdProps =
  testPropertyNamed "Displays command literal" "displayCmdProps"
    $ property
    $ do
      env <- forAll genEnvDispCmd
      log@MkLog {cmd = Just (MkCommandP _ cmd')} <- forAll LGens.genLogWithCmd
      let result = formatConsoleLog env log
      includesOrTruncated (Utils.stripControlAll cmd') result

displayKeyProps :: TestTree
displayKeyProps =
  testPropertyNamed "Displays command lkey" "displayKeyProps"
    $ property
    $ do
      env <- forAll genEnvDispKey
      log@MkLog {cmd = Just (MkCommandP (Just key) _)} <- forAll LGens.genLogWithCmdKey
      let result = formatConsoleLog env log
      includesOrTruncated (Utils.stripControlAll key) result

cmdTruncProps :: TestTree
cmdTruncProps =
  testPropertyNamed "Truncates long command" "cmdTruncProps"
    $ property
    $ do
      (env, cmdNameTrunc) <- forAll genEnvCmdTrunc
      let env' = set' #cmdTrunc (Just cmdNameTrunc) env
      cmdTxt <- forAll genLongCmdText
      log <- forAll LGens.genLog

      let log' = set' #cmd (Just (MkCommandP Nothing cmdTxt)) log
          result = formatConsoleLog env' log'
          cmdNameTruncInt = unsafeConvertIntegral $ cmdNameTrunc ^. #unTruncation

      annotate $ T.unpack result

      -- NOTE: This probably seems weird. We are generating a string s and
      -- truncation t such that length s > t, so why do this check? This is due
      -- to truncation occurring _after_ we strip control chars from the
      -- command name. We only perform truncation if we have
      -- length (stripped s) > t.
      if T.length (Utils.stripControlAll cmdTxt) > cmdNameTruncInt
        then -- violated trunc limit, should be truncated
          assert $ "...]" `T.isInfixOf` result
        else -- under trunc limit, text should be preserved exactly
          assert $ cmdTxt `T.isInfixOf` result

lineTruncProps :: TestTree
lineTruncProps =
  testPropertyNamed "Truncates long line" "lineTruncProps"
    $ property
    $ do
      env <- forAll genEnvLineTrunc
      msg' <- forAll genLongLineText
      log <- forAll LGens.genLog

      -- only perform line truncation for LevelCommand (also requires a command)
      let log' = log {msg = msg', cmd = Just (MkCommandP (Just "") ""), lvl = LevelCommand}
          result = formatConsoleLog env log'

      annotate $ T.unpack result
      assert $ "..." `T.isInfixOf` result
      diff result (\t l -> T.length t < l + colorLen) lineTruncLimit

formatConsoleLog :: Env -> Log -> Text
formatConsoleLog env =
  view #unConsoleLog
    . Formatting.formatConsoleLog keyHide consoleLogging
  where
    keyHide = env ^. #keyHide
    consoleLogging =
      MkConsoleLoggingP
        { cmdLogging = False,
          cmdNameTrunc = env ^. #cmdTrunc,
          lineTrunc = env ^. #lineTrunc,
          -- Important: Tests rely on this being None. Previously it was Smart,
          -- but we didn't use it because cmdLogging was False. Now, however,
          -- we always use this field. Thus, in order for tests to pass without
          -- changing them, we use None.
          stripControl = StripControlNone
        }

-- Colorization adds chars that the shell interprets as color commands.
-- This affects the length, so if we do anything that tests the length
-- of a line, this needs to be taken into account.
--
-- The colorization looks like: \ESC[<digits>m ... \ESC[0m, where digit is up
-- to 3 chars. Strictly speaking, System.Console.Pretty only appears to use
-- two digit colors, i.e. 9 total, and in fact we passed 1,000,000 tests using
-- 9. Still, the standard mentions up to 3 digits, so we will use that, giving
-- a total of 10. More info:
-- https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit
colorLen :: Int
colorLen = 10

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

fileLogProps :: TestTree
fileLogProps =
  T.testGroup
    "File logs"
    [ timestampProps,
      fileLogMessageProps,
      fileLogPrefixProps,
      commandProps,
      commandPropsShowKey,
      shapeProps,
      testFileLogCmdNameTrunc
    ]

timestampProps :: TestTree
timestampProps =
  testPropertyNamed "Starts with timestamp" "timestampProps"
    $ property
    $ do
      log <- forAll LGens.genLog
      let result = formatFileLog KeyHideOn log
          (res, rest) = Utils.breakStripPoint sysTimeNE result
      annotate $ T.unpack result
      annotate $ T.unpack rest
      "" === res

fileLogMessageProps :: TestTree
fileLogMessageProps =
  testPropertyNamed "Includes message" "fileLogMessageProps"
    $ property
    $ do
      log@MkLog {msg} <- forAll LGens.genLog
      let result = formatFileLog KeyHideOn log
      annotate $ T.unpack result
      assert $ T.isInfixOf (T.strip msg) result

fileLogPrefixProps :: TestTree
fileLogPrefixProps =
  testPropertyNamed "Formats prefix" "fileLogPrefixProps"
    $ property
    $ do
      log@MkLog {lvl} <- forAll LGens.genLog
      let result = formatFileLog KeyHideOn log
      let pfx = Formatting.levelToPrefix lvl
      annotate $ T.unpack pfx
      assert $ T.isInfixOf pfx result

commandProps :: TestTree
commandProps =
  testPropertyNamed "Formats command" "commandProps"
    $ property
    $ do
      log@MkLog {cmd = Just (MkCommandP _ cmd')} <- forAll LGens.genLogWithCmd
      let cmdTxt = "[" <> Utils.stripControlAll cmd' <> "]"
          result = formatFileLog KeyHideOn log
      annotate $ T.unpack cmdTxt
      annotate $ T.unpack result
      assert $ T.isInfixOf cmdTxt result

commandPropsShowKey :: TestTree
commandPropsShowKey =
  testPropertyNamed "Formats command with KeyHideOff" "commandProps"
    $ property
    $ do
      log@MkLog {cmd = Just (MkCommandP mk cmd')} <- forAll LGens.genLogWithCmd
      let cmdTxt = case mk of
            Nothing -> "[" <> Utils.stripControlAll cmd' <> "]"
            Just k -> "[" <> Utils.stripControlAll k <> "]"
          result = formatFileLog KeyHideOff log
      annotate $ T.unpack cmdTxt
      annotate $ T.unpack result
      assert $ T.isInfixOf cmdTxt result

shapeProps :: TestTree
shapeProps =
  testPropertyNamed "Formats shape" "shapeProps"
    $ property
    $ do
      log@MkLog {cmd, msg} <- forAll LGens.genLogWithCmd
      let result = formatFileLog KeyHideOn log
          expected =
            mconcat
              [ Formatting.brackets False sysTime,
                Formatting.brackets False (Formatting.logToPrefix log),
                Formatting.brackets True (Utils.stripControlAll $ maybe "received Nothing" (view #command) cmd),
                T.strip msg,
                "\n"
              ]
      annotate $ T.unpack expected
      annotate $ T.unpack result
      expected === result

testFileLogCmdNameTrunc :: TestTree
testFileLogCmdNameTrunc = testCase "File Log Cmd Name Trunc" $ do
  let longLog = mkLog "some long cmd"
      longResult = formatFile' (Just 10) KeyHideOff longLog

  "[2020-05-31 12:00:00][Command][some lo...] cmd\n" @=? longResult

  let shortLog = mkLog "short cmd"
      shortResult = formatFile' (Just 10) KeyHideOff shortLog

  "[2020-05-31 12:00:00][Command][short cmd] cmd\n" @=? shortResult
  where
    mkLog c = MkLog (Just c) "cmd" LevelCommand LogModeSet

formatFileLog :: KeyHide -> Log -> Text
formatFileLog = formatFile' Nothing

formatFile' :: Maybe (Truncation TCmdName) -> KeyHide -> Log -> Text
formatFile' mCmdNameTrunc keyHide log =
  view #unFileLog
    $ Formatting.formatFileLog @MockTime keyHide fileLogging' log
    ^. #runMockTime
  where
    fileLogging' = set' #cmdNameTrunc mCmdNameTrunc fileLogging

fileLogging :: FileLoggingEnv
fileLogging =
  MkFileLoggingP
    { file =
        MkFileLogOpened
          { handle = err "handle",
            queue = err "queue"
          },
      cmdNameTrunc = Nothing,
      stripControl = StripControlNone
    }
  where
    err f =
      error
        $ "[Unit.Props.Shrun.Logging.Queue]: Unit tests should not be using fileLogOpened."
        ++ f

stripCharsSpecs :: TestTree
stripCharsSpecs =
  testGroup
    "stripChars"
    [ stripNone,
      stripAll,
      stripSmart
    ]

stripNone :: TestTree
stripNone =
  testCase "StripControlNone should only strip external whitespace" $ do
    "" @=? stripNone' ""
    "\ESC[ foo \ESC[A  bar \n baz" @=? stripNone' " \n \ESC[ foo \ESC[A  bar \n baz \t  "
  where
    stripNone' = flip Formatting.stripChars StripControlNone

stripAll :: TestTree
stripAll =
  testCase "StripControlAll should strip whitespace + all control" $ do
    "" @=? stripAll' ""
    "oo    bar baz" @=? stripAll' " \n \ESC[ foo \ESC[A \ESC[K  bar \n baz \t  "
  where
    stripAll' = flip Formatting.stripChars StripControlAll

stripSmart :: TestTree
stripSmart =
  testCase "StripControlSmart should strip whitespace + some control" $ do
    "" @=? stripSmart' ""
    "foo \ESC[m   bar baz" @=? stripSmart' " \n \ESC[G foo \ESC[m \ESC[X  bar \n baz \t  "
  where
    stripSmart' = flip Formatting.stripChars StripControlSmart

-- Tests that either the expected string is in the result, or we have
-- done some truncation
includesOrTruncated :: Text -> Text -> PropertyT IO ()
includesOrTruncated expected result = do
  let b1 = T.strip expected `T.isInfixOf` result
      b2 = "..." `T.isInfixOf` result

  annotateShow b1
  annotateShow b2
  assert (b1 || b2)
