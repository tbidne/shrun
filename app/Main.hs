module Main (main) where

import ShellRunner qualified as Sh
import ShellRunner.Types (Command (..))

main :: IO ()
main = Sh.runCommands cmds Nothing
  where
    cmds =
      MkCommand
        <$> [ "sleep 5 && echo hi",
              "sleep2 && sdfkljs",
              "sleep 7 && echo there"
            ]
