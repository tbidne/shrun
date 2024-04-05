module Unit.Shrun.Data.Text (tests) where

import Shrun.Data.Text qualified as Shrun.Text
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.Data.Text"
    [ testStripLinesSep
    ]

testStripLinesSep :: TestTree
testStripLinesSep = testCase "Separates lines by single whitespace" $ do
  "" @=? Shrun.Text.stripLinesSep ""
  "" @=? Shrun.Text.stripLinesSep "\n\n\n"
  "ab def" @=? Shrun.Text.stripLinesSep "\n ab\n def  \n\n"
  "a b" @=? Shrun.Text.stripLinesSep "a  \n\n  b  "
