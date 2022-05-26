{-# LANGUAGE TemplateHaskell #-}

-- | Specs for ShellRun.Utils.
module Unit.Specs.ShellRun.IO (specs) where

import ShellRun.Env.Types (StripControl (..))
import ShellRun.IO qualified as IO
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as THU
import Unit.Prelude

-- | Entry point for ShellRun.Utils specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.IO"
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
    stripNone' = flip IO.stripChars' StripControlNone

stripAll :: TestTree
stripAll =
  THU.testCase "StripControlAll should strip whitespace + all control" $ do
    "" @=? stripAll' ""
    "[ foo [A [K  bar  baz" @=? stripAll' " \n \ESC[ foo \ESC[A \ESC[K  bar \n baz \t  "
  where
    stripAll' = flip IO.stripChars' StripControlAll

stripSmart :: TestTree
stripSmart =
  THU.testCase "StripControlSmart should strip whitespace + some control" $ do
    "" @=? stripSmart' ""
    "foo  \ESC[X  bar \n baz" @=? stripSmart' " \n \ESC[G foo \ESC[A \ESC[X  bar \n baz \t  "
  where
    stripSmart' = flip IO.stripChars' StripControlSmart
