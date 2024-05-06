-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args.Parsing.CommonLogging (tests) where

import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (KeyHideOn),
  )
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.CommonLogging"
    [ keyHideTests
    ]

keyHideTests :: TestTree
keyHideTests =
  testGroup
    "--common-log-key-hide"
    [ testKeyHide,
      testNoKeyHide
    ]

testKeyHide :: TestTree
testKeyHide =
  testPropertyNamed "Parses --common-log-key-hide" "testKeyHide"
    $ U.verifyResult argList expected
  where
    argList = ["--common-log-key-hide", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #keyHide) KeyHideOn

testNoKeyHide :: TestTree
testNoKeyHide =
  testPropertyNamed "Parses --no-common-log-key-hide" "testNoKeyHide"
    $ U.verifyResult argList expected
  where
    argList = ["--no-common-log-key-hide", "command"]
    expected = U.disableDefCoreArgs (#commonLogging % #keyHide)
