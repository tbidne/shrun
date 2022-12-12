-- | Provides 'Log' formatting functionality.
--
-- @since 0.1
module Shrun.Logging.Formatting
  ( -- * High-level
    formatConsoleLog,

    -- * Low-level
    displayCmd,
    displayCmd',
    stripChars',
  )
where

import Data.Text qualified as T
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    HasLogging (..),
    StripControl (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Logging.Types (Log (..), LogLevel (..))
import Shrun.Logging.Types qualified as Log
import Shrun.Prelude
import Shrun.Utils qualified as U
import Shrun.Utils qualified as Utils
import System.Console.Pretty qualified as P

-- | Formats a log to be printed to the console.
--
-- @since 0.1
formatConsoleLog :: (HasLogging env, MonadReader env m) => Log -> m Text
formatConsoleLog log = do
  cmdNameTrunc <- asks getCmdLogNameTrunc
  cmdLineTrunc <- asks getCmdLogLineTrunc
  msg' <- stripChars $ log ^. #msg
  case log ^. #cmd of
    Nothing -> pure $ colorize $ prefix <> " " <> msg'
    Just com -> do
      -- get cmd name to display
      name <- displayCmd com
      let -- truncate cmd/name if necessary
          name' = case cmdNameTrunc ^? _Just % #unTruncation of
            Nothing -> name
            Just n -> U.truncateIfNeeded n name

          -- truncate entire if necessary (flag on and command log only)
          line = colorize $ prefix <> "[" <> name' <> "] " <> msg'
          line' = case (log ^. #lvl, cmdLineTrunc ^? _Just % #unTruncation) of
            (LevelSubCommand, Just m) -> U.truncateIfNeeded m line
            _ -> line
       in pure line'
  where
    colorize = P.color $ Log.logToColor log
    prefix = Log.logToPrefix log
{-# INLINEABLE formatConsoleLog #-}

-- | Variant of 'displayCmd\'' using 'MonadReader'.
--
-- @since 0.1
displayCmd :: (HasLogging env, MonadReader env m) => Command -> m Text
displayCmd cmd = asks getCmdDisplay <&> displayCmd' cmd
{-# INLINEABLE displayCmd #-}

-- | Pretty show for 'Command'. If the command has a key, and 'CmdDisplay' is
-- 'ShowKey' then we return the key. Otherwise we return the command itself.
--
-- >>> displayCmd' (MkCommand Nothing "some long command") HideKey
-- "some long command"
--
-- >>> displayCmd' (MkCommand Nothing "some long command") ShowKey
-- "some long command"
--
-- >>> displayCmd' (MkCommand (Just "long") "some long command") HideKey
-- "some long command"
--
-- >>> displayCmd' (MkCommand (Just "long") "some long command") ShowKey
-- "long"
--
-- @since 0.1
displayCmd' :: Command -> CmdDisplay -> Text
displayCmd' (MkCommand (Just key) _) ShowKey = key
displayCmd' (MkCommand _ cmd) _ = cmd
{-# INLINEABLE displayCmd' #-}

-- We always strip leading/trailing whitespace. Additional options concern
-- internal control chars.
stripChars :: (HasLogging env, MonadReader env m) => Text -> m Text
stripChars txt = stripChars' txt <$> asks getCmdLogStripControl
{-# INLINE stripChars #-}

-- | Applies the given 'StripControl' to the 'Text'.
--
-- * 'StripControlAll': Strips whitespace + all control chars.
-- * 'StripControlSmart': Strips whitespace + 'ansi control' chars.
-- * 'StripControlNone': Strips whitespace.
--
-- @since 0.3
stripChars' :: Text -> Maybe StripControl -> Text
stripChars' txt = \case
  Just StripControlAll -> Utils.stripControlAll txt
  -- whitespace
  Just StripControlNone -> T.strip txt
  --  default to smart
  _ -> Utils.stripControlSmart txt
{-# INLINE stripChars' #-}
