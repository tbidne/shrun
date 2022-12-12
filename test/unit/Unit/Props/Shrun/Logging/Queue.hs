{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Property tests for Shrun.Logging.Queue.
--
-- @since 0.1
module Unit.Props.Shrun.Logging.Queue
  ( props,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.String (IsString)
import Data.Text qualified as T
import Data.Time (ZonedTime (..))
import Data.Time.LocalTime (midday, utc)
import Effects.MonadTime (LocalTime (..), MonadTime (..))
import Hedgehog qualified as H
import Refined qualified as R
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    HasLogging (..),
    StripControl (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Logging.Queue (LogText (..))
import Shrun.Logging.Queue qualified as Queue
import Shrun.Logging.Types (Log (..))
import Shrun.Logging.Types qualified as Log
import Shrun.Utils qualified as Utils
import Test.Tasty qualified as T
import Text.Read qualified as TR
import Unit.Prelude
import Unit.Props.Shrun.Logging.Generators qualified as LGens

-- The mock time our 'MonadTime' returns.
sysTime :: IsString a => a
sysTime = "2022-02-20 23:47:39"

-- Refined variant of 'sysTime'. Includes brackets around the string
-- for use when checking formatting.
sysTimeNE :: Refined R.NonEmpty Text
sysTimeNE = $$(R.refineTH "[2022-02-20 23:47:39]")

newtype MockEnv = MkMockEnv ()

instance HasLogging MockEnv where
  getCmdDisplay = const ShowKey
  getCmdLogLineTrunc = const Nothing
  getCmdLogging = const False
  getCmdLogStripControl = const $ Just StripControlSmart
  getCmdLogNameTrunc = const Nothing
  getFileLogging = const Nothing

  -- This is what we need to run the tests. It's set to
  -- None since our generators can generate null bytes,
  -- thus properties would fail unless we prevent that.
  getFileLogStripControl = const $ Just StripControlNone
  getDisableLogging = const False

-- Monad with mock implementation for 'MonadTime'.
newtype MockTime a = MkMockTime
  { runMockTime :: a
  }
  deriving stock (Eq, Show)
  deriving (Applicative, Functor, Monad) via Identity

instance MonadTime MockTime where
  getSystemTime = pure $ TR.read sysTime
  getSystemZonedTime = pure $ ZonedTime (LocalTime (toEnum 59_000) midday) utc
  getMonotonicTime = pure 0

instance MonadReader MockEnv MockTime where
  ask = pure $ MkMockEnv ()
  local _ = id

makeFieldLabelsNoPrefix ''MockTime

-- | Entry point for Shrun.Logging.Queue property tests.
props :: TestTree
props =
  T.testGroup
    "Shrun.Logging.Queue"
    [ formattingProps
    ]

formattingProps :: TestTree
formattingProps =
  T.testGroup
    "Formatting"
    [ timestampProps,
      messageProps,
      prefixProps,
      commandProps,
      shapeProps
    ]

timestampProps :: TestTree
timestampProps =
  testPropertyNamed "Starts with timestamp" "timestampProps" $
    H.property $ do
      log <- H.forAll LGens.genLog
      let MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
          (res, _) = Utils.breakStripPoint sysTimeNE result
      "" === res

messageProps :: TestTree
messageProps =
  testPropertyNamed "Includes message" "messageProps" $
    H.property $ do
      log@MkLog {msg} <- H.forAll LGens.genLog
      let MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
      H.annotate $ T.unpack result
      H.assert $ T.isInfixOf (T.strip msg) result

prefixProps :: TestTree
prefixProps =
  testPropertyNamed "Formats prefix" "prefixProps" $
    H.property $ do
      log@MkLog {lvl} <- H.forAll LGens.genLog
      let MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
      let pfx = Log.levelToPrefix lvl
      H.annotate $ T.unpack pfx
      H.assert $ T.isInfixOf pfx result

commandProps :: TestTree
commandProps =
  testPropertyNamed "Formats command" "commandProps" $
    H.property $ do
      log@MkLog {cmd = Just (MkCommand _ cmd')} <- H.forAll LGens.genLogWithCmd
      let cmdTxt = "[" <> cmd' <> "]"
          MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
      H.annotate $ T.unpack result
      H.assert $ T.isInfixOf cmdTxt result

shapeProps :: TestTree
shapeProps =
  testPropertyNamed "Formats shape" "shapeProps" $
    H.property $ do
      log@MkLog {cmd, msg} <- H.forAll LGens.genLogWithCmd
      let MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
          expected =
            "["
              <> sysTime
              <> "]"
              <> Log.logToPrefix log
              <> "["
              <> maybe "test error: did not receive Just command" (view #command) cmd
              <> "] "
              <> T.strip msg
              <> "\n"
      H.annotate $ T.unpack expected
      H.annotate $ T.unpack result
      expected === result
