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
    FileLogging (..),
    HasLogging (..),
    Logging (..),
    StripControl (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Logging.Formatting qualified as LFormat
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
      let result = view #unFileLog $ LFormat.formatFileLog @MockTime fileLogging log ^. #runMockTime
          (res, _) = Utils.breakStripPoint sysTimeNE result
      "" === res

messageProps :: TestTree
messageProps =
  testPropertyNamed "Includes message" "messageProps" $
    H.property $ do
      log@MkLog {msg} <- H.forAll LGens.genLog
      let result = view #unFileLog $ LFormat.formatFileLog @MockTime fileLogging log ^. #runMockTime
      H.annotate $ T.unpack result
      H.assert $ T.isInfixOf (T.strip msg) result

prefixProps :: TestTree
prefixProps =
  testPropertyNamed "Formats prefix" "prefixProps" $
    H.property $ do
      log@MkLog {lvl} <- H.forAll LGens.genLog
      let result = view #unFileLog $ LFormat.formatFileLog @MockTime fileLogging log ^. #runMockTime
      let pfx = Log.levelToPrefix lvl
      H.annotate $ T.unpack pfx
      H.assert $ T.isInfixOf pfx result

commandProps :: TestTree
commandProps =
  testPropertyNamed "Formats command" "commandProps" $
    H.property $ do
      log@MkLog {cmd = Just (MkCommand _ cmd')} <- H.forAll LGens.genLogWithCmd
      let cmdTxt = "[" <> cmd' <> "]"
          result = view #unFileLog $ LFormat.formatFileLog @MockTime fileLogging log ^. #runMockTime
      H.annotate $ T.unpack result
      H.assert $ T.isInfixOf cmdTxt result

shapeProps :: TestTree
shapeProps =
  testPropertyNamed "Formats shape" "shapeProps" $
    H.property $ do
      log@MkLog {cmd, msg} <- H.forAll LGens.genLogWithCmd
      let result = view #unFileLog $ LFormat.formatFileLog @MockTime fileLogging log ^. #runMockTime
          expected =
            mconcat
              [ LFormat.brackets False sysTime,
                LFormat.brackets False (Log.logToPrefix log),
                LFormat.brackets True (maybe "received Nothing" (view #command) cmd),
                T.strip msg,
                "\n"
              ]
      H.annotate $ T.unpack expected
      H.annotate $ T.unpack result
      expected === result

fileLogging :: FileLogging
fileLogging = MkFileLogging StripControlNone (error err)
  where
    err = "[Unit.Props.Shrun.Logging.Queue]: Unit tests should not be using fileLogging"
