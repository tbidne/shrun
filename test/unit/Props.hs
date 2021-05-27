{-# LANGUAGE ImportQualifiedPost #-}

module Props (props) where

import Props.ShellRun.Math.NonNegative qualified as NonNegative
import Props.ShellRun.Math.Positive qualified as Positive
import Props.ShellRun.Parsing.Commands qualified as Commands
import Props.ShellRun.Parsing.Legend.Internal qualified as LegendInternal
import Props.ShellRun.Utils qualified as Utils
import Props.ShellRun.Utils.Text qualified as TextUtils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

props :: TestTree
props =
  T.testGroup
    "Hedgehog Properties"
    [ Commands.props,
      LegendInternal.props,
      NonNegative.props,
      Positive.props,
      TextUtils.props,
      Utils.props
    ]
