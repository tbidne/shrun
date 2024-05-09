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
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Notify.Types (NotifyAction, NotifySystemArgs, NotifyTimeout)
import Shrun.Notify.Types qualified as Notify
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

notifyActionParser :: Parser (WithDisabled NotifyAction)
notifyActionParser = Utils.withDisabledParser mainParser "notify-action"
  where
    mainParser =
      OA.optional
        $ OA.option (Notify.parseNotifyAction OA.str)
        $ mconcat
          [ OA.long "notify-action",
            Utils.mkHelp helpTxt,
            OA.metavar Notify.notifyActionStr
          ]
    helpTxt =
      mconcat
        [ "Sends notifications for various actions. 'Final' sends off a ",
          "notification when Shrun itself finishes whereas 'command' sends ",
          "off one each time a command finishes. 'All' implies 'final' and ",
          "'command'."
        ]

notifySystemParser :: Parser (WithDisabled NotifySystemArgs)
notifySystemParser = Utils.withDisabledParser mainParser "notify-system"
  where
    mainParser =
      OA.optional
        $ OA.option (Notify.parseNotifySystem OA.str)
        $ mconcat
          [ OA.long "notify-system",
            Utils.mkHelp helpTxt,
            OA.metavar Notify.notifySystemStr
          ]
    helpTxt =
      mconcat
        [ "The system used for sending notifications. 'dbus' and 'notify-send' ",
          "available on linux, whereas 'apple-script' is available for osx."
        ]

notifyTimeoutParser :: Parser (WithDisabled NotifyTimeout)
notifyTimeoutParser = Utils.withDisabledParser mainParser "notify-timeout"
  where
    mainParser =
      OA.optional
        $ OA.option (Notify.parseNotifyTimeout OA.str)
        $ mconcat
          [ OA.long "notify-timeout",
            Utils.mkHelp helpTxt,
            OA.metavar Notify.notifyTimeoutStr
          ]
    helpTxt =
      mconcat
        [ "When to timeout success notifications. Defaults to 10 seconds.",
          "Note that the underlying notification system may not support ",
          "timeouts."
        ]
