module Props (props) where

import Props.ShellRun.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

props :: TestTree
props =
  T.testGroup
    "Hedgehog Properties"
    [ Utils.props
    ]