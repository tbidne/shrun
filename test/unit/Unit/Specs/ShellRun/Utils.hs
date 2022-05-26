{-# LANGUAGE TemplateHaskell #-}

-- | Specs for ShellRun.Utils.
module Unit.Specs.ShellRun.Utils (specs) where

import Refined (NonEmpty)
import Refined qualified as R
import ShellRun.Utils qualified as Utils
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as THU
import Unit.Prelude

-- | Entry point for ShellRun.Utils specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Utils"
    [ breakStripPointSpecs,
      stripAnsiControlSpecs
    ]

breakStripPointSpecs :: TestTree
breakStripPointSpecs =
  Tasty.testGroup
    "Text"
    [ missingKey,
      stripKey,
      multiBreaksFirst,
      leadingKey,
      trailingKey
    ]

missingKey :: TestTree
missingKey =
  THU.testCase "Missing key should return (str, \"\")" $
    ("ab", "") @=? Utils.breakStripPoint point "ab"

stripKey :: TestTree
stripKey =
  THU.testCase "Normal case should strip out key" $
    ("abc", "def") @=? Utils.breakStripPoint point "abc=def"

multiBreaksFirst :: TestTree
multiBreaksFirst =
  THU.testCase "Multiple keys should only break on first)" $
    ("ab", "cd=ef") @=? Utils.breakStripPoint point "ab=cd=ef"

leadingKey :: TestTree
leadingKey =
  THU.testCase "Leading key should return (\"\", str)" $
    ("", "ab") @=? Utils.breakStripPoint point "=ab"

trailingKey :: TestTree
trailingKey =
  THU.testCase "Trailing key should return (str, \"\")" $
    ("ab", "") @=? Utils.breakStripPoint point "ab="

point :: Refined R.NonEmpty Text
point = $$(R.refineTH @NonEmpty @Text "=")

stripAnsiControlSpecs :: TestTree
stripAnsiControlSpecs =
  Tasty.testGroup
    "stripAnsiControl"
    [ ansiEmptyText,
      ansiControl,
      ansiNonControl
    ]

ansiEmptyText :: TestTree
ansiEmptyText =
  THU.testCase "Empty test should be identity" $
    "" @=? Utils.stripAnsiControl ""

ansiControl :: TestTree
ansiControl =
  THU.testCase "Ansi control sequences are stripped" $ do
    "" @=? Utils.stripAnsiControl "\ESC[A"
    "foo" @=? Utils.stripAnsiControl "foo\ESC[A"
    "bar" @=? Utils.stripAnsiControl "\ESC[Abar"
    "foobar\ESC[1mbaz" @=? Utils.stripAnsiControl "foo\ESC[Abar\ESC[1m\ESC[0Kbaz"

ansiNonControl :: TestTree
ansiNonControl =
  THU.testCase "Ansi non-control is not stripped" $ do
    "\ESC[1" @=? Utils.stripAnsiControl "\ESC[1"
    "\ESC[0mfoo" @=? Utils.stripAnsiControl "\ESC[0mfoo"
    "foo\ESC[0mbar" @=? Utils.stripAnsiControl "foo\ESC[0mbar"
