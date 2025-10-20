{-# LANGUAGE QuasiQuotes #-}

-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args.Parsing.FileLogging (tests) where

import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.FileLogging (DeleteOnSuccessSwitch (MkDeleteOnSuccessSwitch), FileLoggingArgs)
import Shrun.Configuration.Data.FileLogging.FileMode
  ( FileMode (FileModeAppend, FileModeRename, FileModeWrite),
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
import Shrun.Configuration.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Configuration.Data.Truncation
  ( LineTruncation (Detected, Undetected),
  )
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
      parseFileLogDisabled
    ]

testFileLogShort :: TestTree
testFileLogShort =
  testPropertyNamed "Parses -f" "testFileLogShort"
    $ U.verifyResult argList expected
  where
    argList = ["-flogfile", "command"]
    expected = updateDefFileLogArgsWD (#file % #path) (FPManual [osp|logfile|])

testFileLog :: TestTree
testFileLog =
  testPropertyNamed desc "testFileLog"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log"
    argList = ["--file-log=logfile", "command"]
    expected = updateDefFileLogArgsWD (#file % #path) (FPManual [osp|logfile|])

testFileLogDefault :: TestTree
testFileLogDefault =
  testPropertyNamed desc "testFileLogDefault"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log default"
    argList = ["--file-log", "default", "command"]
    expected = updateDefFileLogArgsWD (#file % #path) FPDefault

testFileLogShortDefault :: TestTree
testFileLogShortDefault =
  testPropertyNamed "Parses -f default" "testFileLogShortDefault"
    $ U.verifyResult argList expected
  where
    argList = ["-f", "default", "command"]
    expected = updateDefFileLogArgsWD (#file % #path) FPDefault

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

parseFileLogDisabled :: TestTree
parseFileLogDisabled =
  testPropertyNamed "Parse --file-log off" "parseFileLogDisabled"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log", "off", "command"]
    expected = U.disableDefCoreArgs (#fileLogging % #file % #path)

commandNameTruncTests :: TestTree
commandNameTruncTests =
  testGroup
    "--file-log-command-name-trunc"
    [ testCommandNameTrunc,
      testCommandNameTruncUnderscores,
      testCommandNameTruncDisabled
    ]

testCommandNameTrunc :: TestTree
testCommandNameTrunc =
  testPropertyNamed
    "Parses --file-log-command-name-trunc"
    "testCommandNameTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-command-name-trunc", "15", "command"]
    expected = updateDefFileLogArgsWD #commandNameTrunc 15

testCommandNameTruncUnderscores :: TestTree
testCommandNameTruncUnderscores =
  testPropertyNamed
    "Parses --file-log-command-name-trunc with underscores"
    "testCommandNameTruncUnderscores"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-command-name-trunc", "12_500", "command"]
    expected = updateDefFileLogArgsWD #commandNameTrunc 12_500

testCommandNameTruncDisabled :: TestTree
testCommandNameTruncDisabled =
  testPropertyNamed "Parses --file-log-command-name-trunc off" "testCommandNameTruncDisabled"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-command-name-trunc", "off", "command"]
    expected = disableDefFileLogArgsWD #commandNameTrunc

deleteOnSuccessTests :: TestTree
deleteOnSuccessTests =
  testGroup
    "--file-log-delete-on-success"
    [ testDeleteOnSuccess,
      testDeleteOnSuccessFalse
    ]

testDeleteOnSuccess :: TestTree
testDeleteOnSuccess =
  testPropertyNamed
    "Parses --file-log-delete-on-success true"
    "testDeleteOnSuccess"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-delete-on-success", "on", "command"]
    expected = updateDefFileLogArgs #deleteOnSuccess (MkDeleteOnSuccessSwitch True)

testDeleteOnSuccessFalse :: TestTree
testDeleteOnSuccessFalse =
  testPropertyNamed
    "Parses --file-log-delete-on-success false"
    "testDeleteOnSuccess"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-delete-on-success", "off", "command"]
    expected = updateDefFileLogArgs #deleteOnSuccess (MkDeleteOnSuccessSwitch False)

lineTruncTests :: TestTree
lineTruncTests =
  testGroup
    "--file-log-line-trunc"
    [ testLineTrunc,
      testLineTruncUnderscores,
      testLineTruncDetect,
      testLineTruncDisabled
    ]

testLineTrunc :: TestTree
testLineTrunc =
  testPropertyNamed
    "Parses --file-log-line-trunc"
    "testLineTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-line-trunc", "15", "command"]
    expected = updateDefFileLogArgsWD #lineTrunc (Undetected 15)

testLineTruncUnderscores :: TestTree
testLineTruncUnderscores =
  testPropertyNamed
    "Parses --file-log-line-trunc with underscores"
    "testLineTruncUnderscores"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-line-trunc", "1_50", "command"]
    expected = updateDefFileLogArgsWD #lineTrunc (Undetected 150)

testLineTruncDetect :: TestTree
testLineTruncDetect =
  testPropertyNamed desc "testLineTruncDetect"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-line-trunc detect"
    argList = ["--file-log-line-trunc", "detect", "command"]
    expected = updateDefFileLogArgsWD #lineTrunc Detected

testLineTruncDisabled :: TestTree
testLineTruncDisabled =
  testPropertyNamed "Parses --file-log-line-trunc off" "testLineTruncDisabled"
    $ U.verifyResult argList expected
  where
    argList = ["--file-log-line-trunc", "off", "command"]
    expected = disableDefFileLogArgsWD #lineTrunc

modeTests :: TestTree
modeTests =
  testGroup
    "--file-log-mode"
    [ testModeAppend,
      testModeRename,
      testModeWrite
    ]

testModeAppend :: TestTree
testModeAppend =
  testPropertyNamed desc "testModeAppend"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-mode append"
    argList = ["--file-log-mode", "append", "command"]
    expected = updateDefFileLogArgs (#file % #mode) FileModeAppend

testModeRename :: TestTree
testModeRename =
  testPropertyNamed desc "testModeRename"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-mode rename"
    argList = ["--file-log-mode", "rename", "command"]
    expected = updateDefFileLogArgs (#file % #mode) FileModeRename

testModeWrite :: TestTree
testModeWrite =
  testPropertyNamed desc "testModeWrite"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-mode"
    argList = ["--file-log-mode", "write", "command"]
    expected = updateDefFileLogArgs (#file % #mode) FileModeWrite

stripControlTests :: TestTree
stripControlTests =
  testGroup
    "--file-log-strip-control"
    [ testStripControlAll,
      testStripControlNone,
      testStripControlSmart
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
    desc = "Parses --file-log-strip-control off"
    argList = ["--file-log-strip-control", "off", "command"]
    expected = updateDefFileLogArgs #stripControl StripControlNone

testStripControlSmart :: TestTree
testStripControlSmart =
  testPropertyNamed desc "testStripControlSmart"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-strip-control smart"
    argList = ["--file-log-strip-control", "smart", "command"]
    expected = updateDefFileLogArgs #stripControl StripControlSmart

sizeModeTests :: TestTree
sizeModeTests =
  testGroup
    "--file-log-size-mode"
    [ testSizeModeWarn,
      testSizeModeDelete,
      testSizeModeDisabled
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

testSizeModeDisabled :: TestTree
testSizeModeDisabled =
  testPropertyNamed desc "testSizeModeDisabled"
    $ U.verifyResult argList expected
  where
    desc = "Parses --file-log-size-mode off"
    argList = ["--file-log-size-mode", "off", "command"]
    expected =
      updateDefFileLogArgs
        (#file % #sizeMode)
        FileSizeModeNothing

updateDefFileLogArgsWD ::
  forall a.
  Lens' FileLoggingArgs (Maybe (WithDisabled a)) ->
  a ->
  Maybe Args
updateDefFileLogArgsWD l x = (l' ?~ With x) U.defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (Maybe (WithDisabled a))
    l' = _Just % #coreConfig % #fileLogging % l

updateDefFileLogArgs ::
  forall a.
  Lens' FileLoggingArgs (Maybe a) ->
  a ->
  Maybe Args
updateDefFileLogArgs l x = (l' ?~ x) U.defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (Maybe a)
    l' = _Just % #coreConfig % #fileLogging % l

disableDefFileLogArgsWD ::
  forall a.
  Lens' FileLoggingArgs (Maybe (WithDisabled a)) ->
  Maybe Args
disableDefFileLogArgsWD l = (l' ?~ Disabled) U.defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (Maybe (WithDisabled a))
    l' = _Just % #coreConfig % #fileLogging % l
