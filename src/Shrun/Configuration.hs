module Shrun.Configuration
  ( mergeConfig,
  )
where

import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.CmdLogging (mergeCmdLogging)
import Shrun.Configuration.Data.ConfigPhase
  ( WithDisable,
    altDefault,
    altNothing,
    defaultIfDisabled,
    nothingIfDisabled,
  )
import Shrun.Configuration.Data.Core
  ( CoreConfigArgs,
    CoreConfigP
      ( MkCoreConfigP,
        cmdLogSize,
        cmdLogging,
        cmdNameTrunc,
        fileLogging,
        init,
        keyHide,
        notify,
        pollInterval,
        timeout,
        timerFormat
      ),
  )
import Shrun.Configuration.Data.FileLogging (mergeFileLogging)
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig
      ( MkMergedConfig,
        commands,
        coreConfig
      ),
  )
import Shrun.Configuration.Data.Notify (mergeNotifyLogging)
import Shrun.Configuration.Legend qualified as Legend
import Shrun.Configuration.Toml (Toml)
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Data.KeyHide (KeyHide (KeyHideOff))
import Shrun.Data.PollInterval (defaultPollInterval)
import Shrun.Data.TimerFormat (defaultTimerFormat)
import Shrun.Logging.Types (defaultCmdLogSize)
import Shrun.Prelude

-- | Merges Args and Toml together, filling in necessary defaults and
-- doing some light processing.
--
-- We want this function to do as much to prepare the final config as
-- possible. For instance, in addition to filling in defaults, we also process
-- commands via the legend (MonadThrow) and detect the terminal width for
-- command logging's lineTrunc field (MonadTerminal).
--
-- This is very nearly pure, except for the aforementioned effects.
-- The only remaining tasks the Env needs to take care of is IO that we
-- really can't test anyway, such as opening file handles and creating
-- queues.
mergeConfig ::
  ( MonadTerminal m,
    MonadThrow m
  ) =>
  Args ->
  Maybe Toml ->
  m MergedConfig
mergeConfig args mToml = do
  case mToml of
    Nothing -> do
      let commands = MkCommand Nothing <$> cmdsText
      cmdLogging <-
        mergeCmdLogging
          (args ^. #cmdLog)
          (args ^. (#coreConfig % #cmdLogging))
          Nothing
      pure
        $ MkMergedConfig
          { coreConfig =
              MkCoreConfigP
                { timeout = nothingIfDisabled (args ^. (#coreConfig % #timeout)),
                  init = nothingIfDisabled (args ^. (#coreConfig % #init)),
                  keyHide =
                    defaultIfDisabled KeyHideOff (args ^. (#coreConfig % #keyHide)),
                  pollInterval =
                    defaultIfDisabled
                      defaultPollInterval
                      (args ^. (#coreConfig % #pollInterval)),
                  cmdLogSize =
                    defaultIfDisabled
                      defaultCmdLogSize
                      (args ^. (#coreConfig % #cmdLogSize)),
                  timerFormat =
                    defaultIfDisabled
                      defaultTimerFormat
                      (args ^. (#coreConfig % #timerFormat)),
                  cmdNameTrunc =
                    nothingIfDisabled (args ^. (#coreConfig % #cmdNameTrunc)),
                  cmdLogging,
                  fileLogging =
                    mergeFileLogging
                      (args ^. (#coreConfig % #fileLogging))
                      Nothing,
                  notify =
                    mergeNotifyLogging
                      (args ^. (#coreConfig % #notify))
                      Nothing
                },
            commands
          }
    (Just toml) -> do
      commands <- case toml ^. #legend of
        Nothing -> pure $ MkCommand Nothing <$> cmdsText
        Just aliases -> case Legend.linesToMap aliases of
          Right mp -> case Legend.translateCommands mp cmdsText of
            Right cmds -> pure cmds
            Left err -> throwM err
          Left err -> throwM err

      cmdLogging <-
        mergeCmdLogging
          (args ^. #cmdLog)
          (args ^. (#coreConfig % #cmdLogging))
          (toml ^. (#coreConfig % #cmdLogging))

      pure
        $ MkMergedConfig
          { coreConfig =
              MkCoreConfigP
                { timeout =
                    altNothing' #timeout (toml ^. (#coreConfig % #timeout)),
                  init =
                    altNothing' #init (toml ^. (#coreConfig % #init)),
                  keyHide =
                    altDefault'
                      KeyHideOff
                      #keyHide
                      (toml ^. (#coreConfig % #keyHide)),
                  pollInterval =
                    altDefault'
                      defaultPollInterval
                      #pollInterval
                      (toml ^. (#coreConfig % #pollInterval)),
                  cmdLogSize =
                    altDefault'
                      defaultCmdLogSize
                      #cmdLogSize
                      (toml ^. (#coreConfig % #cmdLogSize)),
                  timerFormat =
                    altDefault'
                      defaultTimerFormat
                      #timerFormat
                      (toml ^. (#coreConfig % #timerFormat)),
                  cmdNameTrunc =
                    altNothing'
                      #cmdNameTrunc
                      (toml ^. (#coreConfig % #cmdNameTrunc)),
                  cmdLogging,
                  fileLogging =
                    mergeFileLogging
                      (args ^. (#coreConfig % #fileLogging))
                      (toml ^. (#coreConfig % #fileLogging)),
                  notify =
                    mergeNotifyLogging
                      (args ^. (#coreConfig % #notify))
                      (toml ^. (#coreConfig % #notify))
                },
            commands
          }
  where
    cmdsText = args ^. #commands

    altDefault' :: a -> Lens' CoreConfigArgs (WithDisable (Maybe a)) -> Maybe a -> a
    altDefault' defA l = altDefault defA args (#coreConfig % l)

    altNothing' :: Lens' CoreConfigArgs (WithDisable (Maybe a)) -> Maybe a -> Maybe a
    altNothing' l = altNothing args (#coreConfig % l)
