{-# LANGUAGE TemplateHaskell #-}

-- | Specs for ShellRun.Utils.
module Unit.Specs.ShellRun.Utils (specs) where

import Refined (NonEmpty)
import Refined qualified as R
import ShellRun.Utils qualified as Utils
import Unit.Prelude

-- | Entry point for ShellRun.Utils specs.
specs :: TestTree
specs =
  testGroup
    "ShellRun.Utils"
    [ breakStripPointSpecs,
      stripAnsiControlSpecs
    ]

breakStripPointSpecs :: TestTree
breakStripPointSpecs =
  testGroup
    "Text"
    [ missingKey,
      stripKey,
      multiBreaksFirst,
      leadingKey,
      trailingKey
    ]

missingKey :: TestTree
missingKey =
  testCase "Missing key should return (str, \"\")" $
    ("ab", "") @=? Utils.breakStripPoint point "ab"

stripKey :: TestTree
stripKey =
  testCase "Normal case should strip out key" $
    ("abc", "def") @=? Utils.breakStripPoint point "abc=def"

multiBreaksFirst :: TestTree
multiBreaksFirst =
  testCase "Multiple keys should only break on first)" $
    ("ab", "cd=ef") @=? Utils.breakStripPoint point "ab=cd=ef"

leadingKey :: TestTree
leadingKey =
  testCase "Leading key should return (\"\", str)" $
    ("", "ab") @=? Utils.breakStripPoint point "=ab"

trailingKey :: TestTree
trailingKey =
  testCase "Trailing key should return (str, \"\")" $
    ("ab", "") @=? Utils.breakStripPoint point "ab="

point :: Refined R.NonEmpty Text
point = $$(R.refineTH @NonEmpty @Text "=")

stripAnsiControlSpecs :: TestTree
stripAnsiControlSpecs =
  testGroup
    "Text Stripping"
    [ emptyText,
      allControlStripped,
      someControlStripped
    ]

emptyText :: TestTree
emptyText =
  testCase "Empty test should be identity" $ do
    "" @=? Utils.stripControlAll ""
    "" @=? Utils.stripControlSmart ""

allControlStripped :: TestTree
allControlStripped =
  testCase "All control sequences are stripped" $ do
    "" @=? Utils.stripControlAll "\ESC[A"
    "foo" @=? Utils.stripControlAll "foo\ESC[A"
    "bar" @=? Utils.stripControlAll "\ESC[Abar"
    "foobarbaz" @=? Utils.stripControlAll "\t foo\ESC[Abar\ESC[1m\n\ESC[0Kbaz \v"

someControlStripped :: TestTree
someControlStripped =
  testCase "Some control sequences are not stripped" $ do
    "" @=? Utils.stripControlSmart "\ESC[A"
    "foo" @=? Utils.stripControlSmart "foo\ESC[A"
    "bar" @=? Utils.stripControlSmart "\ESC[Abar"
    "foobar\ESC[1mbaz" @=? Utils.stripControlSmart "\t foo\ESC[Abar\ESC[1m\n\ESC[0Kbaz \v"
    "\ESC[0mfoo" @=? Utils.stripControlSmart "\ESC[0mfoo"
    "foo\ESC[0mbar" @=? Utils.stripControlSmart "foo\ESC[0mbar"
