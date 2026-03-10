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
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

-- | Specs from readme
specs :: IO TestArgs -> TestTree
specs args =
  testGroup
    "Configuration.md examples"
    [ gif,
      core,
      Examples.Core.tests args,
      Examples.CommonLogging.tests,
      Examples.CommandLogging.tests args,
      Examples.ConsoleLogging.tests,
      Examples.FileLogging.tests args,
      Examples.Notify.tests
    ]

gif :: TestTree
gif = testCase "Runs gif example" $ do
  results <- runExitFailure args
  V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "stats",
          "deploy"
        ]
    expected =
      [ withFinishedPrefix (1, 0, 1, 3) "7 seconds",
        withSuccessPrefix "backend",
        withSuccessPrefix "frontend",
        withSuccessPrefix "stats",
        -- NOTE: The final error message is unreliable, so we would occasionally
        -- run into an error where we received a different message. This led
        -- to the mistaken belief that there was a concurrency bug, which is
        -- perhaps technically true, but it is regarding messages we receive,
        -- not from any output getting mangled.
        withErrorPrefix "db",
        withCommandPrefix "backend" "building backend...",
        withCommandPrefix "db" "building database...",
        withCommandPrefix "frontend" "building frontend...",
        withCommandPrefix "stats" "running stats...",
        waitingPrefix,
        "  - ds",
        withTimerPrefix (1, 1, 1, 2) "6 seconds"
      ]

core :: TestTree
core = testCase "Runs core example" $ do
  results <- runExitFailure args
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
        withFinishedPrefix (0, 0, 1, 3) "0 seconds"
      ]
