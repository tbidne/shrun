-- | Provides 'Log' formatting functionality.
--
-- @since 0.1.0.0
module ShellRun.Logging.Formatting
  ( formatConsoleLog,
  )
where

import ShellRun.Command (Command (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Env.Types
  ( CommandDisplay (..),
    HasCmdTruncation (..),
    HasCommandDisplay (..),
    HasLineTruncation (..),
    Truncation (..),
  )
import ShellRun.Logging.Log (Log (..), LogLevel (..))
import ShellRun.Logging.Log qualified as Log
import ShellRun.Prelude
import ShellRun.Utils qualified as U
import System.Console.Pretty qualified as P

-- TO TEST
--
-- 1. No cmd -> anything to test? Could use logToColor, logToPrefix, but eh...
-- 2. cmd ->
--    a. command display
--    b. cmd truncation
--    c. line truncation

-- Prob do different tests, otherwise pretty complicated

-- | Formats a log to be printed to the console.
--
-- @since 0.1.0.0
formatConsoleLog ::
  ( HasCommandDisplay env,
    HasCmdTruncation env,
    HasLineTruncation env,
    MonadReader env m
  ) =>
  Log ->
  m Text
formatConsoleLog log@MkLog {cmd, msg, lvl} = do
  commandDisplay <- asks getCommandDisplay
  MkTruncation cmdTruncation <- asks getCmdTruncation
  MkTruncation lineTruncation <- asks getLineTruncation
  case cmd of
    Nothing -> pure $ colorize $ prefix <> msg
    Just com ->
      let -- get cmd name to display
          name = case (getKey com, commandDisplay) of
            (Just key, ShowKey) -> key
            (_, _) -> command com
          -- truncate cmd/name if necessary
          name' = case cmdTruncation of
            PPosInf -> name
            PFin n -> U.truncateIfNeeded n name
          -- truncate entire if necessary (flag on and command log only)
          line = colorize $ prefix <> "[" <> name' <> "] " <> msg
          line' = case (lvl, lineTruncation) of
            (SubCommand, PFin m) -> U.truncateIfNeeded m line
            _ -> line
       in pure line'
  where
    colorize = P.color $ Log.logToColor log
    prefix = Log.logToPrefix log
