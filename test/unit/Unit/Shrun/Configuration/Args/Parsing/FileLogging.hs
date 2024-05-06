{-# LANGUAGE QuasiQuotes #-}

-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args.Parsing.FileLogging (tests) where

import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.FileLogging (FileLoggingArgs)
import Shrun.Configuration.Data.FileLogging.FileMode (FileMode (FileModeAppend, FileModeWrite))
import Shrun.Configuration.Data.FileLogging.FilePathDefault (FilePathDefault (FPDefault, FPManual))
import Shrun.Configuration.Data.FileLogging.FileSizeMode
  ( FileSizeMode
      ( FileSizeModeDelete,
        FileSizeModeNothing,
        FileSizeModeWarn
      ),
  )
import Shrun.Configuration.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Configuration.Data.Truncation (LineTruncation (Detected, Undetected))
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.FileLogging"
    [ fileLoggingTests,
      commandNameTruncTests,
      deleteOnSuccessTests,
      lineTruncTests,
      modeTests,
      stripControlTests,
      sizeModeTests
    ]

fileLoggingTests :: TestTree
fileLoggingTests =
  testGroup
    "--file-log"
    [ testFileLogShort,
      testFileLog,
      testFileLogDefault,
      testFileLogShortDefault,
      testFileLogShortEmptyFails,
      testFileLogEmptyFails,
      parseNoFileLog
    ]

testFileLogShort :: TestTree
testFileLogShort =
  testPropertyNamed "Parses -f" "testFileLogShort"
    $ U.verifyResult argList expected
  where
    argList = ["-flogfile", "command"]
    expected = updateDefFileLogArgs (#file % #path) (FPManual [osp|logfile|])

testFileLog :: TestTree
testFileLog =
  testPropertyNamed desc "testFileLog"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log"
    argList = ["--file-log=logfile", "command"]
    expected = updateDefFileLogArgs (#file % #path) (FPManual [osp|logfile|])

testFileLogDefault :: TestTree
testFileLogDefault =
  testPropertyNamed desc "testFileLogDefault"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log default"
    argList = ["--file-log", "default", "command"]
    expected = updateDefFileLogArgs (#file % #path) FPDefault

testFileLogShortDefault :: TestTree
testFileLogShortDefault =
  testPropertyNamed "Parses -f default" "testFileLogShortDefault"
    $ U.verifyResult argList expected
  where
    argList = ["-f", "default", "command"]
    expected = updateDefFileLogArgs (#file % #path) FPDefault

testFileLogShortEmptyFails :: TestTree
testFileLogShortEmptyFails =
  testPropertyNamed desc "testFileLogShortEmptyFails"
    $ U.verifyFailure argList
  where
    desc = "Parses empty -f as failure"
    argList = ["-f", "command"]

testFileLogEmptyFails :: TestTree
testFileLogEmptyFails =
  testPropertyNamed desc "testFileLogEmptyFails"
    $ U.verifyFailure argList
  where
    desc = "Parses empty --file-log as failure"
    argList = ["--file-log=", "command"]

parseNoFileLog :: TestTree
parseNoFileLog =
  testPropertyNamed "Parse --no-file-log" "parseNoFileLog"
    $ U.verifyResult argList expected
  where
    argList = ["--no-file-log", "command"]
    expected = U.disableDefCoreArgs (#fileLogging % #file % #path)

commandNameTruncTests :: TestTree
commandNameTruncTests =
  testGroup
    "--file-log-command-name-trunc"
    [ testCommandNameTrunc,
      testNoCommandNameTrunc
    ]

testCommandNameTrunc :: TestTree
testCommandNameTrunc =
  testPropertyNamed
    "Parses --file-log-command-name-trunc"
    "testCommandNameTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-command-name-trunc", "15", "command"]
    expected = updateDefFileLogArgs #commandNameTrunc 15

testNoCommandNameTrunc :: TestTree
testNoCommandNameTrunc =
  testPropertyNamed "Parses --no-file-log-command-name-trunc" "testNoCommandNameTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--no-file-log-command-name-trunc", "command"]
    expected = disableDefFileLogArgs #commandNameTrunc

deleteOnSuccessTests :: TestTree
deleteOnSuccessTests =
  testGroup
    "--file-log-delete-on-success"
    [ testDeleteOnSuccess,
      testNoDeleteOnSuccess
    ]

testDeleteOnSuccess :: TestTree
testDeleteOnSuccess =
  testPropertyNamed
    "Parses --file-log-delete-on-success"
    "testDeleteOnSuccess"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-delete-on-success", "command"]
    expected = updateDefFileLogArgs #deleteOnSuccess ()

testNoDeleteOnSuccess :: TestTree
testNoDeleteOnSuccess =
  testPropertyNamed
    "Parses --no-file-log-delete-on-success"
    "testDeleteOnSuccess"
    $ U.verifyResult argList expected
  where
    argList = ["--no-file-log-delete-on-success", "command"]
    expected = U.disableDefCoreArgs (#fileLogging % #deleteOnSuccess)

lineTruncTests :: TestTree
lineTruncTests =
  testGroup
    "--file-log-line-trunc"
    [ testLineTrunc,
      testLineTruncDetect,
      testNoLineTrunc
    ]

testLineTrunc :: TestTree
testLineTrunc =
  testPropertyNamed
    "Parses --file-log-line-trunc"
    "testLineTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-line-trunc", "15", "command"]
    expected = updateDefFileLogArgs #lineTrunc (Undetected 15)

testLineTruncDetect :: TestTree
testLineTruncDetect =
  testPropertyNamed desc "testLineTruncDetect"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-line-trunc detect"
    argList = ["--file-log-line-trunc", "detect", "command"]
    expected = updateDefFileLogArgs #lineTrunc Detected

testNoLineTrunc :: TestTree
testNoLineTrunc =
  testPropertyNamed "Parses --no-file-log-line-trunc" "testNoLineTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--no-file-log-line-trunc", "command"]
    expected = disableDefFileLogArgs #lineTrunc

modeTests :: TestTree
modeTests =
  testGroup
    "--file-log-mode"
    [ testModeAppend,
      testModeWrite,
      testNoMode
    ]

testModeAppend :: TestTree
testModeAppend =
  testPropertyNamed desc "testModeAppend"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-mode append"
    argList = ["--file-log-mode", "append", "command"]
    expected = updateDefFileLogArgs (#file % #mode) FileModeAppend

testModeWrite :: TestTree
testModeWrite =
  testPropertyNamed desc "testModeWrite"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-mode"
    argList = ["--file-log-mode", "write", "command"]
    expected = updateDefFileLogArgs (#file % #mode) FileModeWrite

testNoMode :: TestTree
testNoMode =
  testPropertyNamed "Parse --no-file-log-mode" "testNoMode"
    $ U.verifyResult argList expected
  where
    argList = ["--no-file-log-mode", "command"]
    expected = U.disableDefCoreArgs (#fileLogging % #file % #mode)

stripControlTests :: TestTree
stripControlTests =
  testGroup
    "--file-log-strip-control"
    [ testStripControlAll,
      testStripControlNone,
      testStripControlSmart,
      testNoStripControl
    ]

testStripControlAll :: TestTree
testStripControlAll =
  testPropertyNamed desc "testStripControlAll"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-strip-control all"
    argList = ["--file-log-strip-control", "all", "command"]
    expected = updateDefFileLogArgs #stripControl StripControlAll

testStripControlNone :: TestTree
testStripControlNone =
  testPropertyNamed desc "testStripControlNone"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-strip-control none"
    argList = ["--file-log-strip-control", "none", "command"]
    expected = updateDefFileLogArgs #stripControl StripControlNone

testStripControlSmart :: TestTree
testStripControlSmart =
  testPropertyNamed desc "testStripControlSmart"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-strip-control smart"
    argList = ["--file-log-strip-control", "smart", "command"]
    expected = updateDefFileLogArgs #stripControl StripControlSmart

testNoStripControl :: TestTree
testNoStripControl =
  testPropertyNamed desc "testNoStripControl"
    $ U.verifyResult argList expected
  where
    desc = "Parses --no-file-log-strip-control"
    argList = ["--no-file-log-strip-control", "command"]
    expected = U.disableDefCoreArgs (#fileLogging % #stripControl)

sizeModeTests :: TestTree
sizeModeTests =
  testGroup
    "--file-log-size-mode"
    [ testSizeModeWarn,
      testSizeModeDelete,
      testSizeModeNothing,
      testNoSizeMode
    ]

testSizeModeWarn :: TestTree
testSizeModeWarn =
  testPropertyNamed desc "testSizeModeWarn"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-size-mode warn"
    argList = ["--file-log-size-mode", "warn 10 gb", "command"]
    expected =
      updateDefFileLogArgs
        (#file % #sizeMode)
        (FileSizeModeWarn $ MkBytes 10_000_000_000)

testSizeModeDelete :: TestTree
testSizeModeDelete =
  testPropertyNamed desc "testSizeModeDelete"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-size-mode delete"
    argList = ["--file-log-size-mode", "delete 2.4Kilobytes", "command"]
    expected =
      updateDefFileLogArgs
        (#file % #sizeMode)
        (FileSizeModeDelete $ MkBytes 2_400)

testSizeModeNothing :: TestTree
testSizeModeNothing =
  testPropertyNamed desc "testSizeModeNothing"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-size-mode nothing"
    argList = ["--file-log-size-mode", "nothing", "command"]
    expected =
      updateDefFileLogArgs
        (#file % #sizeMode)
        FileSizeModeNothing

testNoSizeMode :: TestTree
testNoSizeMode =
  testPropertyNamed "Parses --no-file-log-size-mode" "testNoSizeMode"
    $ U.verifyResult argList expected
  where
    argList = ["--no-file-log-size-mode", "command"]
    expected = U.disableDefCoreArgs (#fileLogging % #file % #sizeMode)

updateDefFileLogArgs ::
  forall a.
  Lens' FileLoggingArgs (WithDisabled a) ->
  a ->
  Maybe Args
updateDefFileLogArgs l x = (l' .~ With x) U.defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % #fileLogging % l

disableDefFileLogArgs ::
  forall a.
  Lens' FileLoggingArgs (WithDisabled a) ->
  Maybe Args
disableDefFileLogArgs l = (l' .~ Disabled) U.defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % #fileLogging % l
