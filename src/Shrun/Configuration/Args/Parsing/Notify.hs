-- | CLI parsing for NotifyArgs
module Shrun.Configuration.Args.Parsing.Notify
  ( notifyParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.ConfigPhase (WithDisable)
import Shrun.Configuration.Data.Notify
  ( NotifyArgs,
    NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Notify.Types (NotifyAction, NotifySystemP1, NotifyTimeout)
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

notifyActionParser :: Parser (WithDisable (Maybe NotifyAction))
notifyActionParser = Utils.withDisableParser mainParser "notify-action"
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

notifySystemParser :: Parser (WithDisable (Maybe NotifySystemP1))
notifySystemParser = Utils.withDisableParser mainParser "notify-system"
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

notifyTimeoutParser :: Parser (WithDisable (Maybe NotifyTimeout))
notifyTimeoutParser = Utils.withDisableParser mainParser "notify-timeout"
  where
    mainParser =
      OA.optional
        $ OA.option (Notify.parseNotifyTimeout OA.str)
        $ mconcat
          [ OA.long "notify-timeout",
            Utils.mkHelp helpTxt,
            OA.metavar Notify.notifyTimeoutStr
          ]
    helpTxt = "When to timeout success notifications. Defaults to 10 seconds."
