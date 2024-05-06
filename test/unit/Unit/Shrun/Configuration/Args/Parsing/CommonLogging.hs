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
    "--log-key-hide"
    [ testKeyHide,
      testNoKeyHide
    ]

testKeyHide :: TestTree
testKeyHide =
  testPropertyNamed "Parses --log-key-hide" "testKeyHide"
    $ U.verifyResult argList expected
  where
    argList = ["--log-key-hide", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #keyHide) KeyHideOn

testNoKeyHide :: TestTree
testNoKeyHide =
  testPropertyNamed "Parses --no-log-key-hide" "testNoKeyHide"
    $ U.verifyResult argList expected
  where
    argList = ["--no-log-key-hide", "command"]
    expected = U.disableDefCoreArgs (#commonLogging % #keyHide)
