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
      [ debugOn,
        debugOff,
        keyHideOn,
        keyHideOff
      ]

debugOn :: ReadStrategyTestParams
debugOn =
  ReadStrategyTestParametricSimple
    "Runs debug example with --common-log-debug"
    run
    args
    (\results -> V.verifyExpected results expected)
  where
    args =
      withBaseArgs
        [ "--common-log-debug",
          "sleep 2"
        ]
    expected =
      [ withDebugPrefix "sleep 2" "Command: 'ShellCommand \". examples/bashrc && sleep 2\"'",
        withSuccessPrefix "sleep 2"
      ]

debugOff :: ReadStrategyTestParams
debugOff =
  ReadStrategyTestParametricSimple
    "Runs debug example with --no-common-log-debug"
    run
    args
    (\results -> V.verifyExpectedUnexpected results expected unexpected)
  where
    args =
      withBaseArgs
        [ "--no-common-log-debug",
          "sleep 2"
        ]
    expected =
      [ withSuccessPrefix "sleep 2"
      ]
    unexpected =
      [ withDebugPrefix "sleep 2" "Command: 'ShellCommand \". examples/bashrc && sleep 2\"'"
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
