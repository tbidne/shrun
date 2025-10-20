-- | CLI parsing for NotifyArgs
module Shrun.Configuration.Args.Parsing.Notify
  ( notifyParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.Notify
  ( NotifyArgs,
    NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Configuration.Data.Notify.Action (NotifyAction)
import Shrun.Configuration.Data.Notify.Action qualified as Action
import Shrun.Configuration.Data.Notify.System (NotifySystemArgs)
import Shrun.Configuration.Data.Notify.System qualified as System
import Shrun.Configuration.Data.Notify.Timeout (NotifyTimeout)
import Shrun.Configuration.Data.Notify.Timeout qualified as Timeout
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude

notifyParser :: Parser NotifyArgs
notifyParser = do
  action <- notifyActionParser
  system <- notifySystemParser
  timeout <- notifyTimeoutParser

  pure
    $ MkNotifyP
      { action,
        system,
        timeout
      }

notifyActionParser :: Parser (Maybe (WithDisabled NotifyAction))
notifyActionParser =
  Utils.mWithDisabledParser
    (OA.str >>= Action.parseNotifyAction)
    opts
    Action.notifyActionStr
  where
    opts =
      [ OA.long "notify-action",
        Utils.mkHelp helpTxt
      ]

    helpTxt =
      mconcat
        [ "Sends notifications for various actions. 'Final' sends off a ",
          "notification when Shrun itself finishes whereas 'command' sends ",
          "off one each time a command finishes. 'All' implies 'final' and ",
          "'command'."
        ]

notifySystemParser :: Parser (Maybe NotifySystemArgs)
notifySystemParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option (System.parseNotifySystem OA.str)
        $ mconcat
          [ OA.long "notify-system",
            Utils.mkHelp helpTxt,
            OA.metavar System.notifySystemStr
          ]
    helpTxt =
      mconcat
        [ "The system used for sending notifications. 'dbus' and 'notify-send' ",
          "are available on linux, whereas 'apple-script' is available for osx."
        ]

notifyTimeoutParser :: Parser (Maybe NotifyTimeout)
notifyTimeoutParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option (Timeout.parseNotifyTimeout OA.str)
        $ mconcat
          [ OA.long "notify-timeout",
            Utils.mkHelpNoLine helpTxt,
            OA.metavar Timeout.notifyTimeoutStr
          ]
    helpTxt =
      mconcat
        [ "When to timeout success notifications. Defaults to 10 seconds. ",
          "Note that the underlying notification system may not support ",
          "timeouts."
        ]
