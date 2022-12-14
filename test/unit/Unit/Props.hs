-- | Runs property tests.
module Unit.Props (props) where

import Unit.Prelude
import Unit.Props.Shrun.Configuration.Legend qualified as Legend
import Unit.Props.Shrun.Logging.Formatting qualified as Logging.Formatting
import Unit.Props.Shrun.Utils qualified as Utils

-- | Entry point for props.
props :: TestTree
props =
  testGroup
    "Hedgehog Properties"
    [ Logging.Formatting.props,
      Legend.props,
      Utils.props
    ]
