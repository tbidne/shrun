module Functional.Examples.CommonLogging (tests) where

import Functional.Prelude
import Test.Shrun.Verifier (ResultText (MkResultText))
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "CommonLogging"
    [ keyHideOn,
      keyHideOff,
      timerFormatDigitalCompact,
      timerFormatDigitalFull,
      timerFormatProseCompact,
      timerFormatProseFull
    ]

keyHideOn :: TestTree
keyHideOn =
  testCase "Runs key hide example with --log-key-hide" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withBaseArgs
        [ "--log-key-hide",
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
keyHideOff =
  testCase "Runs key hide example without --log-key-hide" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
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

timerFormatDigitalCompact :: TestTree
timerFormatDigitalCompact =
  testCase "Runs timer format with digital_compact" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "--log-timer-format",
          "digital_compact",
          "sleep 2"
        ]
    expected =
      [ withTimerPrefix "01"
      ]

timerFormatDigitalFull :: TestTree
timerFormatDigitalFull =
  testCase "Runs timer format with digital_full" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "--log-timer-format",
          "digital_full",
          "sleep 2"
        ]
    expected =
      [ withTimerPrefix "00:00:00:01"
      ]

timerFormatProseCompact :: TestTree
timerFormatProseCompact =
  testCase "Runs timer format with prose_compact" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "--log-timer-format",
          "prose_compact",
          "sleep 2"
        ]
    expected =
      [ withTimerPrefix "1 second"
      ]

timerFormatProseFull :: TestTree
timerFormatProseFull =
  testCase "Runs timer format with prose_full" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "--log-timer-format",
          "prose_full",
          "sleep 2"
        ]
    expected =
      [ withTimerPrefix "0 days, 0 hours, 0 minutes, 1 second"
      ]
