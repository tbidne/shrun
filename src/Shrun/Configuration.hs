module Shrun.Configuration
  ( mergeConfig,
  )
where

import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.CmdLogging (mergeCmdLogging)
import Shrun.Configuration.Data.CommonLogging (mergeCommonLogging)
import Shrun.Configuration.Data.ConsoleLogging (mergeConsoleLogging)
import Shrun.Configuration.Data.Core
  ( CoreConfigArgs,
    CoreConfigP
      ( MkCoreConfigP,
        cmdLogging,
        commonLogging,
        consoleLogging,
        fileLogging,
        init,
        notify,
        timeout
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

      consoleLogging <-
        mergeConsoleLogging
          (args ^. (#coreConfig % #consoleLogging))
          Nothing
      pure
        $ MkMergedConfig
          { coreConfig =
              MkCoreConfigP
                { timeout = WD.toMaybe (args ^. (#coreConfig % #timeout)),
                  init = WD.toMaybe (args ^. (#coreConfig % #init)),
                  commonLogging =
                    mergeCommonLogging
                      (args ^. (#coreConfig % #commonLogging))
                      Nothing,
                  consoleLogging,
                  cmdLogging =
                    mergeCmdLogging
                      (args ^. (#coreConfig % #cmdLogging))
                      Nothing,
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

      consoleLogging <-
        mergeConsoleLogging
          (args ^. (#coreConfig % #consoleLogging))
          (toml ^. (#coreConfig % #consoleLogging))

      pure
        $ MkMergedConfig
          { coreConfig =
              MkCoreConfigP
                { timeout =
                    plusNothing #timeout (toml ^. (#coreConfig % #timeout)),
                  init =
                    plusNothing #init (toml ^. (#coreConfig % #init)),
                  commonLogging =
                    mergeCommonLogging
                      (args ^. (#coreConfig % #commonLogging))
                      (toml ^. (#coreConfig % #commonLogging)),
                  consoleLogging,
                  cmdLogging =
                    mergeCmdLogging
                      (args ^. (#coreConfig % #cmdLogging))
                      (toml ^. (#coreConfig % #cmdLogging)),
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

    plusNothing :: Lens' CoreConfigArgs (WithDisabled a) -> Maybe a -> Maybe a
    plusNothing l r = WD.toMaybe $ (args ^. (#coreConfig % l)) <>? r
