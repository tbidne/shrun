module Shrun.Configuration
  ( mergeConfig,
  )
where

import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.CmdLogging (mergeCmdLogging)
import Shrun.Configuration.Data.Core
  ( CoreConfigArgs,
    CoreConfigP
      ( MkCoreConfigP,
        cmdLogReadSize,
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
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<>?))
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Configuration.Legend qualified as Legend
import Shrun.Configuration.Toml (Toml)
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Data.KeyHide (KeyHide (KeyHideOff), defaultKeyHide)
import Shrun.Data.PollInterval (defaultPollInterval)
import Shrun.Data.TimerFormat (defaultTimerFormat)
import Shrun.Logging.Types (defaultCmdLogReadSize)
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
                { timeout = WD.toMaybe (args ^. (#coreConfig % #timeout)),
                  init = WD.toMaybe (args ^. (#coreConfig % #init)),
                  keyHide =
                    WD.fromWithDisabled
                      defaultKeyHide
                      (args ^. (#coreConfig % #keyHide)),
                  pollInterval =
                    WD.fromWithDisabled
                      defaultPollInterval
                      (args ^. (#coreConfig % #pollInterval)),
                  cmdLogReadSize =
                    WD.fromWithDisabled
                      defaultCmdLogReadSize
                      (args ^. (#coreConfig % #cmdLogReadSize)),
                  timerFormat =
                    WD.fromWithDisabled
                      defaultTimerFormat
                      (args ^. (#coreConfig % #timerFormat)),
                  cmdNameTrunc =
                    WD.toMaybe (args ^. (#coreConfig % #cmdNameTrunc)),
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
                    plusNothing #timeout (toml ^. (#coreConfig % #timeout)),
                  init =
                    plusNothing #init (toml ^. (#coreConfig % #init)),
                  keyHide =
                    plusDefault
                      KeyHideOff
                      #keyHide
                      (toml ^. (#coreConfig % #keyHide)),
                  pollInterval =
                    plusDefault
                      defaultPollInterval
                      #pollInterval
                      (toml ^. (#coreConfig % #pollInterval)),
                  cmdLogReadSize =
                    plusDefault
                      defaultCmdLogReadSize
                      #cmdLogReadSize
                      (toml ^. (#coreConfig % #cmdLogReadSize)),
                  timerFormat =
                    plusDefault
                      defaultTimerFormat
                      #timerFormat
                      (toml ^. (#coreConfig % #timerFormat)),
                  cmdNameTrunc =
                    plusNothing
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

    plusDefault :: a -> Lens' CoreConfigArgs (WithDisabled a) -> Maybe a -> a
    plusDefault defA l r = WD.fromWithDisabled defA $ (args ^. (#coreConfig % l)) <>? r

    plusNothing :: Lens' CoreConfigArgs (WithDisabled a) -> Maybe a -> Maybe a
    plusNothing l r = WD.toMaybe $ (args ^. (#coreConfig % l)) <>? r
