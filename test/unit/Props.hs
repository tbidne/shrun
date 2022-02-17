-- | Runs property tests.
module Props (props) where

import Props.ShellRun.Command qualified as Command
import Props.ShellRun.Data.TimeRep qualified as Data.TimeRep
import Props.ShellRun.Env qualified as Env
import Props.ShellRun.Legend.Internal qualified as Legend.Internal
import Props.ShellRun.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

-- | Entry point for props.
props :: TestTree
props =
  T.testGroup
    "Hedgehog Properties"
    [ Command.props,
      Data.TimeRep.props,
      Env.props,
      Legend.Internal.props,
      Utils.props
    ]
