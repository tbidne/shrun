-- | Specs for ShellRun.Env.
module Specs.ShellRun.Env (specs) where

import ShellRun.Command (Command (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Env (CommandDisplay (..), CommandTruncation (..))
import ShellRun.Env qualified as Env
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | Entry point for ShellRun.TimeRep specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Env"
    [ displayCommandSpecs,
      displayCommandTruncationSpecs
    ]

displayCommandSpecs :: TestTree
displayCommandSpecs =
  Tasty.testGroup
    "displayCommand"
    [ noKeyShowsCommand,
      showCommand,
      showKey
    ]

noKeyShowsCommand :: TestTree
noKeyShowsCommand = THU.testCase "should use command when no key exists" $ do
  "cmd" @=? Env.displayCommand ShowCommand (MkCommand Nothing "cmd")
  "cmd" @=? Env.displayCommand ShowKey (MkCommand Nothing "cmd")

showCommand :: TestTree
showCommand =
  THU.testCase "should use command with ShowCommand" $
    "cmd" @=? Env.displayCommand ShowCommand (MkCommand (Just "key") "cmd")

showKey :: TestTree
showKey =
  THU.testCase "should use key with ShowKey when one exists" $
    "key" @=? Env.displayCommand ShowKey (MkCommand (Just "key") "cmd")

displayCommandTruncationSpecs :: TestTree
displayCommandTruncationSpecs =
  Tasty.testGroup
    "displayCommandTruncation"
    [ truncatesCommand,
      doesNotTruncateCommand
    ]

truncatesCommand :: TestTree
truncatesCommand = THU.testCase "should truncate command" $ do
  let limit = MkCommandTruncation (PFin 10)
      cmd = MkCommand Nothing "some long command"
  "some lo..." @=? Env.displayCommandTruncation limit ShowCommand cmd

doesNotTruncateCommand :: TestTree
doesNotTruncateCommand = THU.testCase "should not truncate command" $ do
  let limit = MkCommandTruncation PPosInf
      cmd = MkCommand Nothing "some long command"
  "some long command" @=? Env.displayCommandTruncation limit ShowCommand cmd
