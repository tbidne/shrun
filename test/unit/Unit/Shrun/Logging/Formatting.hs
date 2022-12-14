{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests for Shrun.Logging.Formatting.
--
-- @since 0.1
module Unit.Shrun.Logging.Formatting (tests) where

import Data.Functor.Identity (Identity (..))
import Data.String (IsString)
import Data.Text qualified as T
import Data.Time (midday)
import Data.Time.LocalTime (utc)
import Effects.MonadTime (LocalTime (..), MonadTime (..), ZonedTime (..))
import Hedgehog.Gen qualified as HGen
import Hedgehog.Internal.Range qualified as HRange
import Refined qualified as R
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    FileLogging (..),
    HasLogging (..),
    Logging (..),
    StripControl (..),
    TruncRegion (..),
    Truncation (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.Types (Log (..), LogLevel (..))
import Shrun.Utils qualified as Utils
import Test.Tasty qualified as T
import Text.Read qualified as TR
import Unit.Prelude
import Unit.Shrun.Logging.Generators qualified as LGens

data Env = MkEnv
  { cmdDisplay :: CmdDisplay,
    cmdTrunc :: Maybe (Truncation 'TCmdName),
    lineTrunc :: Maybe (Truncation 'TCmdLine)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Env

genEnv :: Gen Env
genEnv =
  MkEnv
    <$> HGen.enumBounded
    <*> (fmap . fmap) MkTruncation genMInt
    <*> (fmap . fmap) MkTruncation genMInt

cmdTruncLimit :: Integral a => a
cmdTruncLimit = 30

genEnvCmdTrunc :: Gen Env
genEnvCmdTrunc =
  MkEnv
    <$> HGen.enumBounded
    <*> fmap (Just . MkTruncation) genNat
    <*> pure Nothing
  where
    genNat = HGen.integral range
    range = HRange.linear 0 cmdTruncLimit

genLongCmdText :: Gen Text
genLongCmdText = HGen.text range HGen.latin1
  where
    range = HRange.linearFrom (cmdTruncLimit + 1) (cmdTruncLimit + 1) 100

lineTruncLimit :: Integral a => a
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
genLongLineText = HGen.text range HGen.latin1
  where
    range = HRange.linearFrom (lineTruncLimit + 1) (lineTruncLimit + 1) 120

genEnvDispCmd :: Gen Env
genEnvDispCmd =
  MkEnv HideKey
    <$> (fmap . fmap) MkTruncation genMInt
    <*> (fmap . fmap) MkTruncation genMInt

genEnvDispKey :: Gen Env
genEnvDispKey =
  MkEnv ShowKey
    <$> (fmap . fmap) MkTruncation genMInt
    <*> (fmap . fmap) MkTruncation genMInt

genMInt :: Gen (Maybe Natural)
genMInt = HGen.choice [Just <$> genNat, pure Nothing]
  where
    genNat = HGen.integral range
    range = HRange.exponential 0 100

instance HasLogging Env () where
  getLogging env =
    MkLogging
      { cmdDisplay = env ^. #cmdDisplay,
        cmdNameTrunc = env ^. #cmdTrunc,
        cmdLogging =
          Just
            MkCmdLogging
              { stripControl = StripControlNone,
                lineTrunc = env ^. #lineTrunc
              },
        consoleLogging = error err,
        fileLogging = Nothing
      }
    where
      err = "[Unit.Props.Shrun.Logging.Formatting]: Unit tests should not be using consoleLogging"

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
      prefixProps,
      displayCmdProps,
      displayKeyProps,
      cmdTruncProps,
      lineTruncProps
    ]

messageProps :: TestTree
messageProps =
  testPropertyNamed "Includes message" "messageProps" $
    property $ do
      env <- forAll genEnv
      log@MkLog {msg} <- forAll LGens.genLog
      let result = formatConsoleLog env log
      annotate $ T.unpack result
      assert $ T.strip msg `T.isInfixOf` result || "..." `T.isInfixOf` result

prefixProps :: TestTree
prefixProps =
  testPropertyNamed "Formats prefix" "prefixProps" $
    property $ do
      env <- forAll genEnv
      log@MkLog {lvl} <- forAll LGens.genLog
      let result = formatConsoleLog env log
      annotate $ "Result: " <> T.unpack result
      let pfx = Formatting.levelToPrefix lvl
      annotate $ T.unpack pfx
      assert $ pfx `T.isInfixOf` result || "..." `T.isInfixOf` result

displayCmdProps :: TestTree
displayCmdProps =
  testPropertyNamed "Displays command literal" "displayCmdProps" $
    property $ do
      env <- forAll genEnvDispCmd
      log@MkLog {cmd = Just (MkCommand _ cmd')} <- forAll LGens.genLogWithCmd
      let result = formatConsoleLog env log
      annotate $ "Result: " <> T.unpack result
      assert $ cmd' `T.isInfixOf` result || "..." `T.isInfixOf` result

displayKeyProps :: TestTree
displayKeyProps =
  testPropertyNamed "Displays command lkey" "displayKeyProps" $
    property $ do
      env <- forAll genEnvDispKey
      log@MkLog {cmd = Just (MkCommand (Just key) _)} <- forAll LGens.genLogWithCmdKey
      let result = formatConsoleLog env log
      annotate $ "Result: " <> T.unpack result
      assert $ key `T.isInfixOf` result || "..." `T.isInfixOf` result

cmdTruncProps :: TestTree
cmdTruncProps =
  testPropertyNamed "Truncates long command" "cmdTruncProps" $
    property $ do
      env <- forAll genEnvCmdTrunc
      cmd' <- MkCommand Nothing <$> forAll genLongCmdText
      log <- forAll LGens.genLog
      let log' = log {cmd = Just cmd'}
          result = formatConsoleLog env log'
      annotate $ "Result: " <> T.unpack result
      assert $ "...]" `T.isInfixOf` result

lineTruncProps :: TestTree
lineTruncProps =
  testPropertyNamed "Truncates long line" "lineTruncProps" $
    property $ do
      env <- forAll genEnvLineTrunc
      msg' <- forAll genLongLineText
      log <- forAll LGens.genLog

      -- only perform line truncation for LevelSubCommand (also requires a command)
      let log' = log {msg = msg', cmd = Just (MkCommand (Just "") ""), lvl = LevelSubCommand}
          result = formatConsoleLog env log'

      annotate $ "Result: " <> T.unpack result
      assert $ "..." `T.isInfixOf` result
      diff result (\t l -> T.length t < l + colorLen) lineTruncLimit

formatConsoleLog :: forall env. HasLogging env () => env -> Log -> Text
formatConsoleLog env =
  view #unConsoleLog
    . Formatting.formatConsoleLog (getLogging @env @() env)

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

-- The mock time our 'MonadTime' returns.
sysTime :: IsString a => a
sysTime = "2022-02-20 23:47:39"

-- Refined variant of 'sysTime'. Includes brackets around the string
-- for use when checking formatting.
sysTimeNE :: Refined R.NonEmpty Text
sysTimeNE = $$(R.refineTH "[2022-02-20 23:47:39]")

newtype MockEnv = MkMockEnv ()

instance HasLogging MockEnv () where
  getLogging _ =
    MkLogging
      { cmdDisplay = ShowKey,
        cmdNameTrunc = Nothing,
        cmdLogging = Nothing,
        consoleLogging = error err,
        fileLogging = Nothing
      }
    where
      err = "[Unit.Props.Shrun.Logging.Queue]: Unit tests should not be using consoleLogging"

-- Monad with mock implementation for 'MonadTime'.
newtype MockTime a = MkMockTime
  { runMockTime :: a
  }
  deriving stock (Eq, Generic, Show)
  deriving (Applicative, Functor, Monad) via Identity

instance MonadTime MockTime where
  getSystemTime = pure $ TR.read sysTime
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
      shapeProps
    ]

timestampProps :: TestTree
timestampProps =
  testPropertyNamed "Starts with timestamp" "timestampProps" $
    property $ do
      log <- forAll LGens.genLog
      let result = formatFileLog log
          (res, _) = Utils.breakStripPoint sysTimeNE result
      "" === res

fileLogMessageProps :: TestTree
fileLogMessageProps =
  testPropertyNamed "Includes message" "messageProps" $
    property $ do
      log@MkLog {msg} <- forAll LGens.genLog
      let result = formatFileLog log
      annotate $ T.unpack result
      assert $ T.isInfixOf (T.strip msg) result

fileLogPrefixProps :: TestTree
fileLogPrefixProps =
  testPropertyNamed "Formats prefix" "prefixProps" $
    property $ do
      log@MkLog {lvl} <- forAll LGens.genLog
      let result = formatFileLog log
      let pfx = Formatting.levelToPrefix lvl
      annotate $ T.unpack pfx
      assert $ T.isInfixOf pfx result

commandProps :: TestTree
commandProps =
  testPropertyNamed "Formats command" "commandProps" $
    property $ do
      log@MkLog {cmd = Just (MkCommand _ cmd')} <- forAll LGens.genLogWithCmd
      let cmdTxt = "[" <> cmd' <> "]"
          result = formatFileLog log
      annotate $ T.unpack result
      assert $ T.isInfixOf cmdTxt result

shapeProps :: TestTree
shapeProps =
  testPropertyNamed "Formats shape" "shapeProps" $
    property $ do
      log@MkLog {cmd, msg} <- forAll LGens.genLogWithCmd
      let result = formatFileLog log
          expected =
            mconcat
              [ Formatting.brackets False sysTime,
                Formatting.brackets False (Formatting.logToPrefix log),
                Formatting.brackets True (maybe "received Nothing" (view #command) cmd),
                T.strip msg,
                "\n"
              ]
      annotate $ T.unpack expected
      annotate $ T.unpack result
      expected === result

formatFileLog :: Log -> Text
formatFileLog log =
  view #unFileLog $
    Formatting.formatFileLog @MockTime fileLogging log ^. #runMockTime

fileLogging :: FileLogging
fileLogging = MkFileLogging StripControlNone (error err)
  where
    err = "[Unit.Props.Shrun.Logging.Queue]: Unit tests should not be using fileLogging"

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
    stripNone' = flip Formatting.stripChars (Just StripControlNone)

stripAll :: TestTree
stripAll =
  testCase "StripControlAll should strip whitespace + all control" $ do
    "" @=? stripAll' ""
    "oo    bar  baz" @=? stripAll' " \n \ESC[ foo \ESC[A \ESC[K  bar \n baz \t  "
  where
    stripAll' = flip Formatting.stripChars (Just StripControlAll)

stripSmart :: TestTree
stripSmart =
  testCase "StripControlSmart should strip whitespace + some control" $ do
    "" @=? stripSmart' ""
    "foo \ESC[m   bar  baz" @=? stripSmart' " \n \ESC[G foo \ESC[m \ESC[X  bar \n baz \t  "
  where
    stripSmart' = flip Formatting.stripChars (Just StripControlSmart)
