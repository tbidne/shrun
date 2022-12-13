{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Property tests for Shrun.Logging.Formatting.
--
-- @since 0.1
module Unit.Props.Shrun.Logging.Formatting
  ( props,
  )
where

import Data.Text qualified as T
import Hedgehog.Gen qualified as HGen
import Hedgehog.Internal.Range qualified as HRange
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    HasLogging (..),
    Logging (..),
    StripControl (..),
    TruncRegion (..),
    Truncation (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.Types (Log (..), LogLevel (..))
import Shrun.Logging.Types qualified as Log
import Test.Tasty qualified as T
import Unit.Prelude
import Unit.Props.Shrun.Logging.Generators qualified as LGens

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
props :: TestTree
props =
  T.testGroup
    "Shrun.Logging.Formatting"
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
      let result = formatLog env log
      annotate $ T.unpack result
      assert $ T.strip msg `T.isInfixOf` result || "..." `T.isInfixOf` result

prefixProps :: TestTree
prefixProps =
  testPropertyNamed "Formats prefix" "prefixProps" $
    property $ do
      env <- forAll genEnv
      log@MkLog {lvl} <- forAll LGens.genLog
      let result = formatLog env log
      annotate $ "Result: " <> T.unpack result
      let pfx = Log.levelToPrefix lvl
      annotate $ T.unpack pfx
      assert $ pfx `T.isInfixOf` result || "..." `T.isInfixOf` result

displayCmdProps :: TestTree
displayCmdProps =
  testPropertyNamed "Displays command literal" "displayCmdProps" $
    property $ do
      env <- forAll genEnvDispCmd
      log@MkLog {cmd = Just (MkCommand _ cmd')} <- forAll LGens.genLogWithCmd
      let result = formatLog env log
      annotate $ "Result: " <> T.unpack result
      assert $ cmd' `T.isInfixOf` result || "..." `T.isInfixOf` result

displayKeyProps :: TestTree
displayKeyProps =
  testPropertyNamed "Displays command lkey" "displayKeyProps" $
    property $ do
      env <- forAll genEnvDispKey
      log@MkLog {cmd = Just (MkCommand (Just key) _)} <- forAll LGens.genLogWithCmdKey
      let result = formatLog env log
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
          result = formatLog env log'
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
          result = formatLog env log'

      annotate $ "Result: " <> T.unpack result
      assert $ "..." `T.isInfixOf` result
      diff result (\t l -> T.length t < l + colorLen) lineTruncLimit

formatLog :: forall env. HasLogging env () => env -> Log -> Text
formatLog env =
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
