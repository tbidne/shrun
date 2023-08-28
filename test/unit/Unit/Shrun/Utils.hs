-- | Tests for Shrun.Utils
module Unit.Shrun.Utils (tests) where

import Shrun.Utils qualified as U
import Unit.Generators qualified as PGens
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.Utils"
    [ diffTimeProps,
      breakStripPointSpecs,
      stripAnsiControlSpecs
    ]

diffTimeProps :: TestTree
diffTimeProps =
  testPropertyNamed "diffTime" "diffTimeProps"
    $ property
    $ do
      t1 <- forAll PGens.genTimeSpec
      t2 <- forAll PGens.genTimeSpec
      let result = U.diffTime t1 t2
      assert $ result >= 0

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
  testCase "Missing key should return (str, \"\")"
    $ ("ab", "")
    @=? U.breakStripPoint point "ab"

stripKey :: TestTree
stripKey =
  testCase "Normal case should strip out key"
    $ ("abc", "def")
    @=? U.breakStripPoint point "abc=def"

multiBreaksFirst :: TestTree
multiBreaksFirst =
  testCase "Multiple keys should only break on first)"
    $ ("ab", "cd=ef")
    @=? U.breakStripPoint point "ab=cd=ef"

leadingKey :: TestTree
leadingKey =
  testCase "Leading key should return (\"\", str)"
    $ ("", "ab")
    @=? U.breakStripPoint point "=ab"

trailingKey :: TestTree
trailingKey =
  testCase "Trailing key should return (str, \"\")"
    $ ("ab", "")
    @=? U.breakStripPoint point "ab="

point :: Text
point = "="

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
    "" @=? U.stripControlAll ""
    "" @=? U.stripControlSmart ""

allControlStripped :: TestTree
allControlStripped =
  testCase "All control sequences are stripped" $ do
    "" @=? U.stripControlAll "\ESC[A"
    "foo" @=? U.stripControlAll "foo\ESC[A"
    "bar" @=? U.stripControlAll "\ESC[Abar"
    "foobarbaz" @=? U.stripControlAll "\t foo\ESC[Abar\ESC[1m\n\ESC[0Kbaz \v"

someControlStripped :: TestTree
someControlStripped =
  testCase "Some control sequences are not stripped" $ do
    "" @=? U.stripControlSmart "\ESC[A"
    "foo" @=? U.stripControlSmart "foo\ESC[A"
    "bar" @=? U.stripControlSmart "\ESC[Abar"
    "foobar\ESC[1mbaz" @=? U.stripControlSmart "\t foo\ESC[Abar\ESC[1m\n\ESC[0Kbaz \v"
    "\ESC[0mfoo" @=? U.stripControlSmart "\ESC[0mfoo"
    "foo\ESC[0mbar" @=? U.stripControlSmart "foo\ESC[0mbar"
