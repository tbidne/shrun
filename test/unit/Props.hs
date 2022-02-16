-- | Runs property tests.
module Props (props) where

import Props.ShellRun.Command qualified as Command
import Props.ShellRun.Data.TimeRep qualified as TimeRep
import Props.ShellRun.Legend.Internal qualified as LegendI
import Props.ShellRun.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

-- | Entry point for props.
props :: TestTree
props =
  T.testGroup
    "Hedgehog Properties"
    [ Command.props,
      LegendI.props,
      TimeRep.props,
      Utils.props
    ]
