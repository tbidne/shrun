-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args.Parsing.CommonLogging (tests) where

import Shrun.Configuration.Data.CommonLogging
  ( Debug (MkDebug),
  )
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (MkKeyHideSwitch),
  )
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.CommonLogging"
    [ debugTests,
      keyHideTests
    ]

debugTests :: TestTree
debugTests =
  testGroup
    "--common-log-debug"
    [ testDebug,
      testDebugFalse
    ]

testDebug :: TestTree
testDebug =
  testPropertyNamed "Parses --common-log-debug on" "testDebug"
    $ U.verifyResult argList expected
  where
    argList = ["--common-log-debug", "on", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #debug) (MkDebug True)

testDebugFalse :: TestTree
testDebugFalse =
  testPropertyNamed "Parses --common-log-debug false" "testDebugFalse"
    $ U.verifyResult argList expected
  where
    argList = ["--common-log-debug", "off", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #debug) (MkDebug False)

keyHideTests :: TestTree
keyHideTests =
  testGroup
    "--common-log-key-hide"
    [ testKeyHide,
      testKeyHideFalse
    ]

testKeyHide :: TestTree
testKeyHide =
  testPropertyNamed "Parses --common-log-key-hide on" "testKeyHide"
    $ U.verifyResult argList expected
  where
    argList = ["--common-log-key-hide", "on", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #keyHide) (MkKeyHideSwitch True)

testKeyHideFalse :: TestTree
testKeyHideFalse =
  testPropertyNamed "Parses --common-log-key-hide false" "testKeyHideFalse"
    $ U.verifyResult argList expected
  where
    argList = ["--common-log-key-hide", "off", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #keyHide) (MkKeyHideSwitch False)
