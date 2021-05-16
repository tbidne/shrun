module ShellRunner.Types
  ( Command (..),
    module T,
  )
where

import Data.Text (Text)
import ShellRunner.Types.IO as T
import ShellRunner.Types.NonNegative as T
import ShellRunner.Types.Positive as T

newtype Command = MkCommand {getCommand :: Text}