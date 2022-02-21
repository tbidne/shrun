-- | Runs property tests.
module Props (props) where

import Props.ShellRun.Command qualified as Command
import Props.ShellRun.Data.TimeRep qualified as Data.TimeRep
import Props.ShellRun.Legend.Internal qualified as Legend.Internal
import Props.ShellRun.Logging.Formatting qualified as Logging.Formatting
import Props.ShellRun.Logging.Queue qualified as Logging.Queue
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
      Logging.Formatting.props,
      Legend.Internal.props,
      Logging.Queue.props,
      Utils.props
    ]
