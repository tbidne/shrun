-- | Specs for ShellRun.Data.Env.
module Specs.ShellRun.Data.Env (specs) where

import ShellRun.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
import ShellRun.Data.Env qualified as Env
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | Entry point for ShellRun.TimeRep specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Data.Env"
    [ displayCommandSpecs
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
