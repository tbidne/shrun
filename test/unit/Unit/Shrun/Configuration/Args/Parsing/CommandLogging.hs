module Unit.Shrun.Configuration.Args.Parsing.CommandLogging (tests) where

import GHC.Real (fromIntegral)
import Shrun.Configuration.Data.CommandLogging
  ( ReportReadErrorsSwitch (MkReportReadErrorsSwitch),
  )
import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize (MkReadSize))
import Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy (ReadBlock, ReadBlockLineBuffer),
  )
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.CommandLogging"
    [ bufferLengthTests,
      bufferTimeoutTests,
      pollIntervalTests,
      readSizeTests,
      readStrategyTests,
      reportReadErrorsTests
    ]

bufferLengthTests :: TestTree
bufferLengthTests =
  testGroup
    "--command-log-buffer-length"
    [ testBufferLength,
      testBufferLengthOverflow
    ]

testBufferLength :: TestTree
testBufferLength =
  testPropertyNamed
    "Parses --command-log-buffer-length"
    "testBufferLength"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-buffer-length", "2_000", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #bufferLength) 2_000

testBufferLengthOverflow :: TestTree
testBufferLengthOverflow =
  testPropertyNamed
    "--command-log-buffer-length overflow fails"
    "testBufferLengthOverflow"
    $ U.verifyFailure argList
  where
    maxIntInc = fromIntegral @Int @Natural (maxBound @Int) + 1
    maxIntIncArg = show maxIntInc ++ " b"
    argList = ["--command-log-buffer-length", maxIntIncArg, "command"]

bufferTimeoutTests :: TestTree
bufferTimeoutTests =
  testGroup
    "--command-log-buffer-timeout"
    [ testBufferTimeout,
      testBufferTimeoutString
    ]

testBufferTimeout :: TestTree
testBufferTimeout =
  testPropertyNamed
    "Parses --command-log-buffer-timeout"
    "testBufferTimeout"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-buffer-timeout", "2_000", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #bufferTimeout) (fromℤ 2_000)

testBufferTimeoutString :: TestTree
testBufferTimeoutString =
  testPropertyNamed
    "Parses --command-log-buffer-timeout"
    "testBufferTimeout"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-buffer-timeout", "1d2h3m4s", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #bufferTimeout) (fromℤ 93784)

pollIntervalTests :: TestTree
pollIntervalTests =
  testGroup
    "--command-log-poll-interval"
    [ testPollInterval,
      testPollIntervalUnderscores
    ]

testPollInterval :: TestTree
testPollInterval =
  testPropertyNamed desc "testPollInterval"
    $ U.verifyResult argList expected
  where
    desc = "Parses --command-log-poll-interval"
    argList = ["--command-log-poll-interval", "1000", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #pollInterval) 1000

testPollIntervalUnderscores :: TestTree
testPollIntervalUnderscores =
  testPropertyNamed desc "testPollIntervalUnderscores"
    $ U.verifyResult argList expected
  where
    desc = "Parses --command-log-poll-interval with underscores"
    argList = ["--command-log-poll-interval", "1_000_000", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #pollInterval) 1_000_000

readSizeTests :: TestTree
readSizeTests =
  testGroup
    "--command-log-read-size"
    [ testReadSize,
      testReadSizeOverflow
    ]

testReadSize :: TestTree
testReadSize =
  testPropertyNamed
    "Parses --command-log-read-size"
    "testReadSize"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-read-size", "2 kb", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #readSize) (MkReadSize $ MkBytes 2_000)

testReadSizeOverflow :: TestTree
testReadSizeOverflow =
  testPropertyNamed
    "--command-log-read-size overflow fails"
    "testReadSizeOverflow"
    $ U.verifyFailure argList
  where
    maxIntInc = fromIntegral @Int @Natural (maxBound @Int) + 1
    maxIntIncArg = show maxIntInc ++ " b"
    argList = ["--command-log-read-size", maxIntIncArg, "command"]

readStrategyTests :: TestTree
readStrategyTests =
  testGroup
    "--command-log-read-strategy"
    [ testReadStrategyBlock,
      testReadStrategyBlockBufferLine
    ]

testReadStrategyBlock :: TestTree
testReadStrategyBlock =
  testPropertyNamed
    "Parses --command-log-read-strategy block"
    "testReadStrategyBlock"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-read-strategy", "block", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #readStrategy) ReadBlock

testReadStrategyBlockBufferLine :: TestTree
testReadStrategyBlockBufferLine =
  testPropertyNamed
    "Parses --command-log-read-strategy block-line-buffer"
    "testReadStrategyBlockBufferLine"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-read-strategy", "block-line-buffer", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #readStrategy) ReadBlockLineBuffer

reportReadErrorsTests :: TestTree
reportReadErrorsTests =
  testGroup
    "--command-log-report-read-errors"
    [ testReportReadErrors,
      testReportReadErrorsFalse
    ]

testReportReadErrors :: TestTree
testReportReadErrors =
  testPropertyNamed
    "Parses --command-log-report-read-errors true"
    "testReportReadErrors"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-report-read-errors", "on", "command"]
    expected =
      U.updateDefCoreArgs
        (#commandLogging % #reportReadErrors)
        (MkReportReadErrorsSwitch True)

testReportReadErrorsFalse :: TestTree
testReportReadErrorsFalse =
  testPropertyNamed
    "Parses --command-log-report-read-errors false"
    "testReportReadErrorsFalse"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-report-read-errors", "off", "command"]
    expected =
      U.updateDefCoreArgs
        (#commandLogging % #reportReadErrors)
        (MkReportReadErrorsSwitch False)
