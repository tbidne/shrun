module Functional.Examples.CommonLogging (tests) where

import Functional.Prelude
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "CommonLogging"
    (multiTestReadStrategy testsParams)
  where
    testsParams :: List ReadStrategyTestParams
    testsParams =
      [ keyHideOn,
        keyHideOff
      ]

keyHideOn :: ReadStrategyTestParams
keyHideOn =
  ReadStrategyTestParametricSimple
    "Runs key hide example with --common-log-key-hide"
    run
    args
    (\results -> V.verifyExpectedUnexpected results expected unexpected)
  where
    args =
      withBaseArgs
        [ "--common-log-key-hide",
          "some-key"
        ]
    expected =
      [ withCommandPrefix "echo hi && sleep 2" "hi",
        withSuccessPrefix "echo hi && sleep 2"
      ]
    unexpected =
      [ withCommandPrefix "some-key" "",
        withSuccessPrefix "some-key"
      ]

keyHideOff :: ReadStrategyTestParams
keyHideOff =
  ReadStrategyTestParametricSimple
    "Runs key hide example without --common-log-key-hide"
    run
    args
    (\results -> V.verifyExpectedUnexpected results expected unexpected)
  where
    args =
      withBaseArgs
        [ "some-key"
        ]
    expected =
      [ withCommandPrefix "some-key" "hi",
        withSuccessPrefix "some-key"
      ]
    unexpected =
      [ withCommandPrefix "echo hi && sleep 2" "",
        withSuccessPrefix "echo hi && sleep 2"
      ]
