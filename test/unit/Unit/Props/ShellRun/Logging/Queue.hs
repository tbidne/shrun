{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Property tests for ShellRun.Logging.Queue.
--
-- @since 0.1.0.0
module Unit.Props.ShellRun.Logging.Queue
  ( props,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.String (IsString)
import Data.Text qualified as T
import Hedgehog qualified as H
import Refined qualified as R
import ShellRun.Class.MonadTime (MonadTime (..))
import ShellRun.Command (Command (..))
import ShellRun.Logging.Log
  ( Log (..),
    LogLevel (..),
  )
import ShellRun.Logging.Log qualified as Log
import ShellRun.Logging.Queue (LogText (..))
import ShellRun.Logging.Queue qualified as Queue
import ShellRun.Utils qualified as Utils
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Text.Read qualified as TR
import Unit.MaxRuns (MaxRuns (..))
import Unit.Prelude
import Unit.Props.ShellRun.Logging.Generators qualified as LGens

-- The mock time our 'MonadTime' returns.
sysTime :: IsString a => a
sysTime = "2022-02-20 23:47:39.90228065 UTC"

-- Refined variant of 'sysTime'. Includes brackets around the string
-- for use when checking formatting.
sysTimeNE :: Refined R.NonEmpty Text
sysTimeNE = $$(R.refineTH "[2022-02-20 23:47:39.90228065 UTC]")

-- Monad with mock implementation for 'MonadTime'.
newtype MockTime a = MkMockTime
  { runMockTime :: a
  }
  deriving stock (Eq, Show)
  deriving (Applicative, Functor, Monad) via Identity

instance MonadTime MockTime where
  getSystemTime = pure $ TR.read sysTime

makeFieldLabelsNoPrefix ''MockTime

-- | Entry point for ShellRun.Logging.Queue property tests.
props :: TestTree
props =
  T.testGroup
    "ShellRun.Logging.Queue"
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
  TH.testProperty "Starts with timestamp" $
    H.withTests limit $
      H.property $ do
        log <- H.forAll LGens.genLog
        let MkLogText result = Queue.formatFileLog @MockTime log ^. #runMockTime
            (res, _) = Utils.breakStripPoint sysTimeNE result
        "" === res

messageProps :: TestTree
messageProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Includes message" $
    H.withTests limit $
      H.property $ do
        log@MkLog {msg} <- H.forAll LGens.genLog
        let MkLogText result = Queue.formatFileLog @MockTime log ^. #runMockTime
        H.annotate $ T.unpack result
        H.assert $ T.isInfixOf msg result

prefixProps :: TestTree
prefixProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Formats prefix" $
    H.withTests limit $
      H.property $ do
        log@MkLog {lvl} <- H.forAll LGens.genLog
        let MkLogText result = Queue.formatFileLog @MockTime log ^. #runMockTime
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
  TH.testProperty "Formats command" $
    H.withTests limit $
      H.property $ do
        log@MkLog {cmd = Just (MkCommand _ cmd')} <- H.forAll LGens.genLogWithCmd
        let cmdTxt = "[" <> cmd' <> "]"
            MkLogText result = Queue.formatFileLog @MockTime log ^. #runMockTime
        H.annotate $ T.unpack result
        H.assert $ T.isInfixOf cmdTxt result

shapeProps :: TestTree
shapeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Formats shape" $
    H.withTests limit $
      H.property $ do
        log@MkLog {cmd, msg} <- H.forAll LGens.genLogWithCmd
        let MkLogText result = Queue.formatFileLog @MockTime log ^. #runMockTime
            expected =
              "["
                <> sysTime
                <> "] "
                <> Log.logToPrefix log
                <> "["
                <> maybe "test error: did not receive Just command" (view #command) cmd
                <> "] "
                <> msg
                <> "\n"
        H.annotate $ T.unpack expected
        H.annotate $ T.unpack result
        expected === result
