-- | Misc tests
module Functional.Miscellaneous (specs) where

import Functional.Prelude
import Test.Shrun.Verifier (ResultText (MkResultText))
import Test.Shrun.Verifier qualified as V

specs :: TestTree
specs =
  testGroup
    "Miscellaneous"
    [ splitNewlineLogs
    ]

splitNewlineLogs :: TestTree
splitNewlineLogs = testCase "Logs with newlines are split" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withNoConfig
        [ "--cmd-log",
          "sleep 1 && echo 'line one\nline two' && sleep 2"
        ]
    expected =
      [ withSuccessPrefix "sleep 1 && echo 'line oneline two' && sleep 2",
        withCommandPrefix "sleep 1 && echo 'line oneline two' && sleep 2" "line one",
        withCommandPrefix "sleep 1 && echo 'line oneline two' && sleep 2" "line two"
      ]
    unexpected =
      [ withCommandPrefix "sleep 1 && echo 'line oneline two' && sleep 2" "line oneline two"
      ]
