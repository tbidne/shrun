{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module Unit.Shrun.Configuration.Toml (tests) where

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Shrun.Command.Types qualified as CT
import Shrun.Configuration.Data.CommandLogging
  ( BufferLength (MkBufferLength),
    BufferTimeout (MkBufferTimeout),
    CommandLoggingP
      ( MkCommandLoggingP,
        bufferLength,
        bufferTimeout,
        pollInterval,
        readSize,
        readStrategy,
        reportReadErrors
      ),
  )
import Shrun.Configuration.Data.CommandLogging.PollInterval
  ( PollInterval (MkPollInterval),
  )
import Shrun.Configuration.Data.CommandLogging.ReadSize
  ( ReadSize (MkReadSize),
  )
import Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingP (MkCommonLoggingP, debug, keyHide),
    Debug (MkDebug),
  )
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (MkKeyHideSwitch),
  )
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLoggingP
      ( MkConsoleLoggingP,
        commandLogging,
        commandNameTrunc,
        lineTrunc,
        stripControl,
        timerFormat
      ),
  )
import Shrun.Configuration.Data.Core
  ( CoreConfigP
      ( MkCoreConfigP,
        commandLogging,
        commonLogging,
        consoleLogging,
        fileLogging,
        init,
        notify,
        timeout
      ),
    CoreConfigToml,
  )
import Shrun.Configuration.Data.Core.Timeout (Timeout (MkTimeout))
import Shrun.Configuration.Data.FileLogging
  ( FileLogInitP (MkFileLogInitP, mode, path, sizeMode),
    FileLoggingP
      ( MkFileLoggingP,
        commandNameTrunc,
        deleteOnSuccess,
        file,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.FileLogging.FilePathDefault
  ( FilePathDefault (FPDefault, FPManual),
  )
import Shrun.Configuration.Data.FileLogging.FileSizeMode
  ( FileSizeMode
      ( FileSizeModeDelete,
        FileSizeModeNothing,
        FileSizeModeWarn
      ),
  )
import Shrun.Configuration.Data.Graph
  ( EdgeArgs (EdgeArgsList, EdgeArgsSequential),
    Edges (MkEdges),
  )
import Shrun.Configuration.Data.Notify
  ( NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (NotifyTimeoutNever, NotifyTimeoutSeconds),
  )
import Shrun.Configuration.Data.Truncation
  ( LineTruncation (Detected, Undetected),
    Truncation (MkTruncation),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Shrun.Configuration.Toml (Toml (MkToml, coreConfig, legend))
import Shrun.Configuration.Toml.Legend (KeyVal, unsafeKeyVal)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Toml"
    [ lawTests
    ]

lawTests :: TestTree
lawTests =
  testGroup
    "Laws"
    [ testAssociativity,
      testIdentity
    ]

testAssociativity :: TestTree
testAssociativity = testProp desc "testAssociativity" $ do
  x <- forAll genToml
  y <- forAll genToml
  z <- forAll genToml
  (x <> y) <> z === x <> (y <> z)
  where
    desc = "Semigroup associativity"

testIdentity :: TestTree
testIdentity = testProp desc "testIdentity" $ do
  x <- forAll genToml
  x === mempty <> x
  x === x <> mempty
  where
    desc = "Monoid identity"

genToml :: Gen Toml
genToml = do
  coreConfig <- genCoreConfig
  legend <- genLegend
  pure
    $ MkToml
      { coreConfig,
        legend
      }

genCoreConfig :: Gen CoreConfigToml
genCoreConfig = do
  init <- genMWithDisabled genText
  timeout <- genMWithDisabled genTimeout
  commonLogging <- genMaybe genCommonLogging
  commandLogging <- genMaybe genCommandLogging
  consoleLogging <- genMaybe genConsoleLogging
  fileLogging <- genMaybe genFileLogging
  notify <- genMaybe genNotify
  pure
    $ MkCoreConfigP
      { init,
        timeout,
        commonLogging,
        commandLogging,
        consoleLogging,
        fileLogging,
        notify
      }
  where
    genTimeout = MkTimeout <$> genPos

    genCommonLogging = do
      debug <- fmap MkDebug <$> genMaybe G.enumBounded
      keyHide <- fmap MkKeyHideSwitch <$> genMaybe G.enumBounded
      pure
        $ MkCommonLoggingP
          { debug,
            keyHide
          }

    genCommandLogging = do
      bufferLength <- fmap MkBufferLength <$> genMaybe genNat
      bufferTimeout <- fmap MkBufferTimeout <$> genMaybe genTimeout
      pollInterval <- fmap MkPollInterval <$> genMaybe genNat
      readSize <- fmap MkReadSize <$> genMaybe genBytes
      readStrategy <- genMaybe G.enumBounded
      reportReadErrors <- genMaybe G.enumBounded

      pure
        $ MkCommandLoggingP
          { bufferLength,
            bufferTimeout,
            pollInterval,
            readSize,
            readStrategy,
            reportReadErrors
          }

    genConsoleLogging = do
      commandLogging <- genMaybe G.enumBounded
      commandNameTrunc <- genMWithDisabled genTruncation
      lineTrunc <- genMWithDisabled genLineTruncation
      stripControl <- genMaybe G.enumBounded
      timerFormat <- genMaybe G.enumBounded
      pure
        $ MkConsoleLoggingP
          { commandLogging,
            commandNameTrunc,
            lineTrunc,
            stripControl,
            timerFormat
          }

    genFileLogging = do
      file <- do
        path <-
          genMWithDisabled
            $ G.frequency
              [ (1, pure FPDefault),
                (3, FPManual <$> genOsPath)
              ]
        mode <- genMaybe G.enumBounded
        sizeMode <-
          genMaybe
            $ G.choice
              [ pure FileSizeModeNothing,
                FileSizeModeWarn <$> genBytes,
                FileSizeModeDelete <$> genBytes
              ]
        pure
          $ MkFileLogInitP
            { path,
              mode,
              sizeMode
            }

      commandNameTrunc <- genMWithDisabled genTruncation
      deleteOnSuccess <- genMaybe G.enumBounded
      lineTrunc <- genMWithDisabled genLineTruncation
      stripControl <- genMaybe G.enumBounded
      pure
        $ MkFileLoggingP
          { file,
            commandNameTrunc,
            deleteOnSuccess,
            lineTrunc,
            stripControl
          }

    genNotify = do
      action <- genMWithDisabled G.enumBounded
      system <- genMaybe G.enumBounded
      timeout <- genMaybe genNotifyTimeout
      pure
        $ MkNotifyP
          { action,
            system,
            timeout
          }

    genNotifyTimeout =
      G.frequency
        [ (1, pure NotifyTimeoutNever),
          (3, NotifyTimeoutSeconds <$> genNat)
        ]

    genLineTruncation =
      G.choice
        [ pure Detected,
          Undetected <$> genTruncation
        ]
    genTruncation = MkTruncation <$> genNat

genLegend :: Gen (Maybe (Seq KeyVal))
genLegend = genMaybe genSeq
  where
    genSeq = listToSeq <$> G.list (R.linearFrom 0 0 20) genKeyVal

    genKeyVal =
      unsafeKeyVal
        <$> genMEdgeArgs
        <*> genTextNE
        <*> genVals

    genMEdgeArgs = genMaybe genEdgeArgs

    genEdgeArgs =
      G.frequency
        [ (1, pure EdgeArgsSequential),
          (3, EdgeArgsList <$> genEdgeArgsList)
        ]

    genEdgeArgsList =
      MkEdges
        . listToSeq
        <$> G.list (R.linearFrom 1 1 20) genEdge

    genEdge = (,) <$> genCommandIdx <*> genCommandIdx

    genCommandIdx = CT.unsafeFromInt <$> genPos

    genVals = G.list (R.linearFrom 1 1 20) genTextNE

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g =
  G.frequency
    [ (1, pure Nothing),
      (3, Just <$> g)
    ]

genWithDisabled :: Gen a -> Gen (WithDisabled a)
genWithDisabled g =
  G.frequency
    [ (1, pure Disabled),
      (3, With <$> g)
    ]

genMWithDisabled :: Gen a -> Gen (Maybe (WithDisabled a))
genMWithDisabled = genMaybe . genWithDisabled

genBytes :: (Integral a) => Gen (Bytes B a)
genBytes = MkBytes <$> genNat

genNat :: (Integral a) => Gen a
genNat = G.integral @_ (R.linearFrom 0 0 20)

genPos :: (Integral a) => Gen a
genPos = G.integral @_ (R.linearFrom 1 1 20)

genTextNE :: Gen Text
genTextNE = G.text (R.linearFrom 1 1 20) G.unicode

genText :: Gen Text
genText = G.text (R.linearFrom 0 0 20) G.unicode

genOsPath :: Gen OsPath
genOsPath = unsafeEncode . unpack <$> genText
