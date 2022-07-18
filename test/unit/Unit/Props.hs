-- | Runs property tests.
module Unit.Props (props) where

import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Props.ShellRun.Configuration.Legend qualified as Legend
import Unit.Props.ShellRun.Logging.Formatting qualified as Logging.Formatting
import Unit.Props.ShellRun.Logging.Queue qualified as Logging.Queue
import Unit.Props.ShellRun.Utils qualified as Utils

-- | Entry point for props.
props :: TestTree
props =
  T.testGroup
    "Hedgehog Properties"
    [ Logging.Formatting.props,
      Legend.props,
      Logging.Queue.props,
      Utils.props
    ]
