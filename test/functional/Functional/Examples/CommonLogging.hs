module Functional.Examples.CommonLogging (tests) where

import Functional.Prelude
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "CommonLogging"
    [ debugOn,
      debugOff,
      keyHideOn,
      keyHideOff
    ]

debugOn :: TestTree
debugOn = testCase "Runs debug example with --common-log-debug" $ do
  results <- run args
  V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "--common-log-debug",
          "on",
          "sleep 2"
        ]
    expected =
      [ withDebugPrefix "sleep 2" "Command: 'ShellCommand \". examples/bashrc && sleep 2\"'",
        withSuccessPrefix "sleep 2"
      ]

debugOff :: TestTree
debugOff = testCase "Runs debug example with --common-log-debug false" $ do
  results <- run args
  V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withBaseArgs
        [ "--common-log-debug",
          "off",
          "sleep 2"
        ]
    expected =
      [ withSuccessPrefix "sleep 2"
      ]
    unexpected =
      [ withDebugPrefix "sleep 2" "Command: 'ShellCommand \". examples/bashrc && sleep 2\"'"
      ]

keyHideOn :: TestTree
keyHideOn = testCase "Runs key hide example with --common-log-key-hide" $ do
  results <- run args
  V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withBaseArgs
        [ "--common-log-key-hide",
          "on",
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

keyHideOff :: TestTree
keyHideOff = testCase "Runs key hide example without --common-log-key-hide" $ do
  results <- run args
  V.verifyExpectedUnexpected results expected unexpected
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
