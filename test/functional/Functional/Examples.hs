-- | The tests in this module hierarchy are those from the configuration.md
-- (plus a few more, where noted). Thus these should be kept in sync.
module Functional.Examples (specs) where

import Functional.Examples.CommandLogging qualified as Examples.CommandLogging
import Functional.Examples.CommonLogging qualified as Examples.CommonLogging
import Functional.Examples.ConsoleLogging qualified as Examples.ConsoleLogging
import Functional.Examples.Core qualified as Examples.Core
import Functional.Examples.FileLogging qualified as Examples.FileLogging
import Functional.Examples.Notify qualified as Examples.Notify
import Functional.Prelude
import Functional.TestArgs (TestArgs)
import Test.Shrun.Verifier (ResultText (MkResultText))
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

-- | Specs from readme
specs :: IO TestArgs -> TestTree
specs args =
  testGroup
    "Configuration.md examples"
    [ gif,
      core,
      Examples.Core.tests,
      Examples.CommonLogging.tests,
      Examples.CommandLogging.tests,
      Examples.ConsoleLogging.tests,
      Examples.FileLogging.tests args,
      Examples.Notify.tests
    ]

gif :: TestTree
gif =
  testCase "Runs gif example" $ do
    results <- fmap MkResultText <$> runExitFailure args
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "sign-peace-treaty",
          "takeover"
        ]
    expected =
      [ withFinishedPrefix "13 seconds",
        withSuccessPrefix "skynet",
        withSuccessPrefix "ui",
        -- NOTE: The final error message is unreliable, so we would occasionally
        -- run into an error where we received a different message. This led
        -- to the mistaken belief that there was a concurrency bug, which is
        -- perhaps technically true, but it is regarding messages we receive,
        -- not from any output getting mangled.
        withErrorPrefix "sign-peace-treaty",
        withCommandPrefix "skynet" "preparing nuclear missil-- i mean gift baskets",
        withCommandPrefix "ui" "adding emojis. we like to have fun :-)",
        withCommandPrefix "querying-targets" "finding targets...",
        withCommandPrefix "sign-peace-treaty" "play it cool...",
        withTimerPrefix "8 seconds"
      ]

core :: TestTree
core =
  testCase "Runs core example" $ do
    results <- fmap MkResultText <$> runExitFailure args
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "all",
          "echo cat"
        ]
    expected =
      [ withSuccessPrefix "echo cat",
        withSuccessPrefix "echo hi",
        withSuccessPrefix "cmd1",
        withErrorPrefix "cmd4",
        withFinishedPrefix "0 seconds"
      ]
