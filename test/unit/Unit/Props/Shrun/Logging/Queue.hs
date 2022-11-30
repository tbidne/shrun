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
import Data.Int (Int64)
import Data.String (IsString)
import Data.Text qualified as T
import Hedgehog qualified as H
import Refined qualified as R
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    HasLogging (..),
    StripControl (..),
  )
import Shrun.Data.Command (Command (..))
import Effects.MonadTime (MonadTime (..), LocalTime (..))
import Shrun.Logging.Queue (LogText (..))
import Data.Time.LocalTime (midday)
import Shrun.Logging.Queue qualified as Queue
import Shrun.Logging.Types
  ( Log (..),
    LogLevel (..),
  )
import Shrun.Logging.Types qualified as Log
import Shrun.Utils qualified as Utils
import Test.Tasty qualified as T
import Text.Read qualified as TR
import Unit.MaxRuns (MaxRuns (..))
import Unit.Prelude
import Unit.Props.Shrun.Logging.Generators qualified as LGens

-- The mock time our 'MonadTime' returns.
sysTime :: IsString a => a
sysTime = "2022-02-20 23:47:39.90228065 UTC"

-- Refined variant of 'sysTime'. Includes brackets around the string
-- for use when checking formatting.
sysTimeNE :: Refined R.NonEmpty Text
sysTimeNE = $$(R.refineTH "[2022-02-20 23:47:39.90228065 UTC]")

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
  getSystemTime = pure $ LocalTime (toEnum 59_000) midday
  --getSystemZonedTime = pure $ ZonedTime (LocalTime (toEnum 59_000) midday) utc
  getSystemZonedTime = pure $ TR.read sysTime
  getTimeSpec = pure $ fromIntegral @Int64 0

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
timestampProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Starts with timestamp" "timestampProps" $
    H.withTests limit $
      H.property $ do
        log <- H.forAll LGens.genLog
        let MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
            (res, _) = Utils.breakStripPoint sysTimeNE result
        "" === res

messageProps :: TestTree
messageProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Includes message" "messageProps" $
    H.withTests limit $
      H.property $ do
        log@MkLog {msg} <- H.forAll LGens.genLog
        let MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
        H.annotate $ T.unpack result
        H.assert $ T.isInfixOf (T.strip msg) result

prefixProps :: TestTree
prefixProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Formats prefix" "prefixProps" $
    H.withTests limit $
      H.property $ do
        log@MkLog {lvl} <- H.forAll LGens.genLog
        let MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
        case lvl of
          -- level is None: no prefix
          None -> foldr (noMatch result) (pure ()) nonEmptyPrefixes
          -- level is not None: includes prefix
          _ -> do
            let pfx = Log.levelToPrefix lvl
            H.annotate $ T.unpack pfx
            H.assert $ T.isInfixOf pfx result
  where
    nonEmptyPrefixes = [SubCommand .. Fatal]
    noMatch :: Text -> LogLevel -> PropertyT IO () -> PropertyT IO ()
    noMatch t level acc = do
      let pfx = Log.levelToPrefix level
      H.annotate $ T.unpack t
      H.annotate $ T.unpack pfx
      H.assert $ not (T.isInfixOf pfx t)
      acc

commandProps :: TestTree
commandProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Formats command" "commandProps" $
    H.withTests limit $
      H.property $ do
        log@MkLog {cmd = Just (MkCommand _ cmd')} <- H.forAll LGens.genLogWithCmd
        let cmdTxt = "[" <> cmd' <> "]"
            MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
        H.annotate $ T.unpack result
        H.assert $ T.isInfixOf cmdTxt result

shapeProps :: TestTree
shapeProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Formats shape" "shapeProps" $
    H.withTests limit $
      H.property $ do
        log@MkLog {cmd, msg} <- H.forAll LGens.genLogWithCmd
        let MkLogText result = Queue.formatFileLog @_ @MockTime log ^. #runMockTime
            expected =
              "["
                <> sysTime
                <> "] "
                <> Log.logToPrefix log
                <> "["
                <> maybe "test error: did not receive Just command" (view #command) cmd
                <> "] "
                <> T.strip msg
                <> "\n"
        H.annotate $ T.unpack expected
        H.annotate $ T.unpack result
        expected === result
