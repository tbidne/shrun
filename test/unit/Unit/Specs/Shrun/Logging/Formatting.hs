-- | Specs for Shrun.Logging.Formatting.
module Unit.Specs.Shrun.Logging.Formatting (specs) where

import Shrun.Configuration.Env.Types (StripControl (..))
import Shrun.Logging.Formatting qualified as Formatting
import Unit.Prelude

-- | Entry point for Shrun.Formatting specs.
specs :: TestTree
specs =
  testGroup
    "Shrun.Logging.Formatting"
    [ stripCharsSpecs
    ]

stripCharsSpecs :: TestTree
stripCharsSpecs =
  testGroup
    "stripChars"
    [ stripNone,
      stripAll,
      stripSmart
    ]

stripNone :: TestTree
stripNone =
  testCase "StripControlNone should only strip external whitespace" $ do
    "" @=? stripNone' ""
    "\ESC[ foo \ESC[A  bar \n baz" @=? stripNone' " \n \ESC[ foo \ESC[A  bar \n baz \t  "
  where
    stripNone' = flip Formatting.stripChars (Just StripControlNone)

stripAll :: TestTree
stripAll =
  testCase "StripControlAll should strip whitespace + all control" $ do
    "" @=? stripAll' ""
    "oo    bar  baz" @=? stripAll' " \n \ESC[ foo \ESC[A \ESC[K  bar \n baz \t  "
  where
    stripAll' = flip Formatting.stripChars (Just StripControlAll)

stripSmart :: TestTree
stripSmart =
  testCase "StripControlSmart should strip whitespace + some control" $ do
    "" @=? stripSmart' ""
    "foo \ESC[m   bar  baz" @=? stripSmart' " \n \ESC[G foo \ESC[m \ESC[X  bar \n baz \t  "
  where
    stripSmart' = flip Formatting.stripChars (Just StripControlSmart)
