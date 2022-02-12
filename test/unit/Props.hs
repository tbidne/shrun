-- | Runs property tests.
module Props (props) where

import Props.ShellRun.Data.TimeRep qualified as TimeRep
import Props.ShellRun.Parsing.Commands qualified as Commands
import Props.ShellRun.Parsing.Legend.Internal qualified as LegendI
import Props.ShellRun.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

-- | Entry point for props.
props :: TestTree
props =
  T.testGroup
    "Hedgehog Properties"
    [ Commands.props,
      LegendI.props,
      TimeRep.props,
      Utils.props
    ]
