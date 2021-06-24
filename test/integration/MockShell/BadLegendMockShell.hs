{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides the 'GoodMockShell' type.
module MockShell.BadLegendMockShell (BadLegendMockShell (..)) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)
import Data.Text (Text)
import MockEnv (MockEnv)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Logging (MonadLogger (..))
import ShellRun.Data.Legend (LegendErr (..))

-- | 'BadLegendMockShell' is intended to test a run of
-- 'ShellRun.runShell' when the path to the legend file is bad.
newtype BadLegendMockShell a = MkBadLegendMockShell
  {runBadLegendMockShell :: MockShellBase a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader MockEnv,
      MonadWriter [Text],
      MonadLogger
    )
    via MockShellBase

instance MonadShell BadLegendMockShell where
  legendPathToMap _ = pure $ Left $ FileErr "File not found"

  runCommands _ = pure ()

instance Show a => Show (BadLegendMockShell a) where
  show _ = "MkBadLegendMockShell"
