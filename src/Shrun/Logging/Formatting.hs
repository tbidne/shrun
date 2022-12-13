-- | Provides 'Log' formatting functionality.
--
-- @since 0.1
module Shrun.Logging.Formatting
  ( -- * High-level
    formatConsoleLog,
    formatFileLog,

    -- * Low-level
    displayCmd,
    stripChars,
  )
where

import Data.Text qualified as T
import Effects.MonadTime
  ( MonadTime (getSystemTime),
    formatLocalTime,
  )
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    FileLogging,
    Logging (..),
    StripControl (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Logging.Types (Log (..), LogLevel (..))
import Shrun.Logging.Types qualified as Log
import Shrun.Logging.Types.Internal
  ( ConsoleLog (UnsafeConsoleLog),
    FileLog (UnsafeFileLog),
  )
import Shrun.Prelude
import Shrun.Utils qualified as U
import Shrun.Utils qualified as Utils
import System.Console.Pretty qualified as P

-- | Formats a log to be printed to the console.
--
-- @since 0.1
formatConsoleLog :: Logging r -> Log -> ConsoleLog
formatConsoleLog logging log =
  case log ^. #cmd of
    Nothing -> UnsafeConsoleLog $ colorize $ prefix <> " " <> msgStripped
    Just com -> do
      -- get cmd name to display
      let name = displayCmd com (logging ^. #cmdDisplay)
          -- truncate cmd/name if necessary
          name' = truncateNameFn name

          line = colorize $ prefix <> "[" <> name' <> "] " <> msgStripped
          line' = truncateCmdLineFn line
       in UnsafeConsoleLog line'
  where
    msgStripped = stripChars (log ^. #msg) mStripControl
    mCmdLogging = logging ^. #cmdLogging
    mStripControl = mCmdLogging ^? _Just % #stripControl

    truncateNameFn =
      maybeApply
        U.truncateIfNeeded
        (logging ^? (#cmdNameTrunc %? #unTruncation))

    -- truncate entire line if necessary (flag on and command log only)
    truncateCmdLineFn = case ( log ^. #lvl,
                               mCmdLogging ^? (_Just % #lineTrunc %? #unTruncation)
                             ) of
      (LevelSubCommand, Just m) -> U.truncateIfNeeded m
      _ -> id

    colorize = P.color $ Log.logToColor log
    prefix = Log.logToPrefix log

maybeApply :: (a -> b -> b) -> Maybe a -> b -> b
maybeApply = maybe id

-- | Formats a 'Log' into a 'FileLog'. Applies prefix and timestamp.
--
-- @since 0.7
formatFileLog ::
  ( MonadTime m
  ) =>
  FileLogging ->
  Log ->
  m FileLog
formatFileLog fileLogging log = do
  currTime <- formatLocalTime <$> getSystemTime
  let formatted = case log ^. #cmd of
        Nothing -> prefix <> msgStripped
        Just com -> prefix <> "[" <> (com ^. #command) <> "] " <> msgStripped
      withTimestamp = "[" <> pack currTime <> "]" <> formatted <> "\n"
  pure $ UnsafeFileLog withTimestamp
  where
    msgStripped = stripChars (log ^. #msg) (Just stripControl)
    stripControl = fileLogging ^. #stripControl
    prefix = Log.logToPrefix log

-- | Pretty show for 'Command'. If the command has a key, and 'CmdDisplay' is
-- 'ShowKey' then we return the key. Otherwise we return the command itself.
--
-- >>> displayCmd (MkCommand Nothing "some long command") HideKey
-- "some long command"
--
-- >>> displayCmd (MkCommand Nothing "some long command") ShowKey
-- "some long command"
--
-- >>> displayCmd (MkCommand (Just "long") "some long command") HideKey
-- "some long command"
--
-- >>> displayCmd (MkCommand (Just "long") "some long command") ShowKey
-- "long"
--
-- @since 0.1
displayCmd :: Command -> CmdDisplay -> Text
displayCmd (MkCommand (Just key) _) ShowKey = key
displayCmd (MkCommand _ cmd) _ = cmd

-- | Applies the given 'StripControl' to the 'Text'.
--
-- * 'StripControlAll': Strips whitespace + all control chars.
-- * 'StripControlSmart': Strips whitespace + 'ansi control' chars.
-- * 'StripControlNone': Strips whitespace.
--
-- @since 0.3
stripChars :: Text -> Maybe StripControl -> Text
stripChars txt = \case
  Just StripControlAll -> Utils.stripControlAll txt
  -- whitespace
  Just StripControlNone -> T.strip txt
  --  default to smart
  _ -> Utils.stripControlSmart txt
{-# INLINE stripChars #-}
