-- | Specs for ShellRun.Utils.
module Specs.ShellRun.Utils (specs) where

import Refined (Refined)
import Refined qualified as R
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
import ShellRun.Data.TH qualified as TH
import ShellRun.Prelude
import ShellRun.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | Entry point for ShellRun.Utils specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Utils"
    [ displayCommandSpecs,
      breakStripPointSpecs
    ]

displayCommandSpecs :: TestTree
displayCommandSpecs =
  Tasty.testGroup
    "displayCommand"
    [ noKeyShowsCommand,
      showCommand,
      showKey
    ]

noKeyShowsCommand :: TestTree
noKeyShowsCommand = THU.testCase "should use command when no key exists" $ do
  "cmd" @=? Utils.displayCommand ShowCommand (MkCommand Nothing "cmd")
  "cmd" @=? Utils.displayCommand ShowKey (MkCommand Nothing "cmd")

showCommand :: TestTree
showCommand =
  THU.testCase "should use command with ShowCommand" $
    "cmd" @=? Utils.displayCommand ShowCommand (MkCommand (Just "key") "cmd")

showKey :: TestTree
showKey =
  THU.testCase "should use key with ShowKey when one exists" $
    "key" @=? Utils.displayCommand ShowKey (MkCommand (Just "key") "cmd")

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
point = TH.equalsNE
