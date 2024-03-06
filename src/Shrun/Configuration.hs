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
        coreConfig,
        legend
      ),
  )
import Shrun.Configuration.Data.Notify (mergeNotifyLogging)
import Shrun.Configuration.Toml (Toml)
import Shrun.Data.KeyHide (KeyHide (KeyHideOff))
import Shrun.Data.PollInterval (defaultPollInterval)
import Shrun.Data.TimerFormat (defaultTimerFormat)
import Shrun.Logging.Types (defaultCmdLogSize)
import Shrun.Prelude

mergeConfig :: Args -> Maybe Toml -> MergedConfig
mergeConfig args = \case
  Nothing ->
    MkMergedConfig
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
              cmdLogging =
                mergeCmdLogging
                  (args ^. #cmdLog)
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
        legend = Nothing,
        commands = args ^. #commands
      }
  (Just toml) ->
    MkMergedConfig
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
              cmdLogging =
                mergeCmdLogging
                  (args ^. #cmdLog)
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
        legend = toml ^. #legend,
        commands = args ^. #commands
      }
  where
    altDefault' :: a -> Lens' CoreConfigArgs (WithDisable (Maybe a)) -> Maybe a -> a
    altDefault' defA l = altDefault defA args (#coreConfig % l)

    altNothing' :: Lens' CoreConfigArgs (WithDisable (Maybe a)) -> Maybe a -> Maybe a
    altNothing' l = altNothing args (#coreConfig % l)
