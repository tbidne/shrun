{-# LANGUAGE ImportQualifiedPost #-}

module Props (props) where

import Props.ShellRun.Math.NonNegative qualified as NonNegative
import Props.ShellRun.Math.Positive qualified as Positive
import Props.ShellRun.Parsing.Commands qualified as Commands
import Props.ShellRun.Parsing.Legend.Internal qualified as LegendI
import Props.ShellRun.Utils qualified as Utils
import Props.ShellRun.Utils.Internal qualified as UtilsI
import Props.ShellRun.Utils.Text qualified as UtilsText
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

props :: TestTree
props =
  T.testGroup
    "Hedgehog Properties"
    [ Commands.props,
      LegendI.props,
      NonNegative.props,
      Positive.props,
      Utils.props,
      UtilsI.props,
      UtilsText.props
    ]
