-- | Specs for ShellRun.Logging.Formatting.
module Unit.Specs.ShellRun.Logging.Formatting (specs) where

import ShellRun.Env.Types (StripControl (..))
import ShellRun.Logging.Formatting qualified as Formatting
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as THU
import Unit.Prelude

-- | Entry point for ShellRun.Formatting specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Logging.Formatting"
    [ stripChars'Specs
    ]

stripChars'Specs :: TestTree
stripChars'Specs =
  Tasty.testGroup
    "stripChars'"
    [ stripNone,
      stripAll,
      stripSmart
    ]

stripNone :: TestTree
stripNone =
  THU.testCase "StripControlNone should only strip external whitespace" $ do
    "" @=? stripNone' ""
    "\ESC[ foo \ESC[A  bar \n baz" @=? stripNone' " \n \ESC[ foo \ESC[A  bar \n baz \t  "
  where
    stripNone' = flip Formatting.stripChars' StripControlNone

stripAll :: TestTree
stripAll =
  THU.testCase "StripControlAll should strip whitespace + all control" $ do
    "" @=? stripAll' ""
    "oo    bar  baz" @=? stripAll' " \n \ESC[ foo \ESC[A \ESC[K  bar \n baz \t  "
  where
    stripAll' = flip Formatting.stripChars' StripControlAll

stripSmart :: TestTree
stripSmart =
  THU.testCase "StripControlSmart should strip whitespace + some control" $ do
    "" @=? stripSmart' ""
    "foo \ESC[m   bar  baz" @=? stripSmart' " \n \ESC[G foo \ESC[m \ESC[X  bar \n baz \t  "
  where
    stripSmart' = flip Formatting.stripChars' StripControlSmart
