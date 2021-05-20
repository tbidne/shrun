{-# LANGUAGE ImportQualifiedPost #-}

module Props (props) where

import Props.ShellRun.Utils qualified as Utils
import Props.ShellRun.Types.NonNegative qualified as NonNegative
import Props.ShellRun.Types.Positive qualified as Positive
import Props.ShellRun.Parsing.Legend.Internal qualified as LegendInternal
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

props :: TestTree
props =
  T.testGroup
    "Hedgehog Properties"
    [ LegendInternal.props,
      NonNegative.props,
      Positive.props,
      Utils.props
    ]