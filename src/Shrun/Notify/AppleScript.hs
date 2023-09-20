-- | Effect for AppleScript.
module Shrun.Notify.AppleScript
  ( -- * Effect
    AppleScriptDynamic (..),
    notify,

    -- ** Handler
    runAppleScriptDynamicIO,

    -- * Functions
    notifyAppleScript,
  )
where

import Data.Text qualified as T
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.Process.Typed qualified as P
import Shrun.Notify.Notify (ShrunNote)
import Shrun.Prelude

-- | Dynamic effect for apple-script.
data AppleScriptDynamic :: Effect where
  Notify :: Text -> AppleScriptDynamic es ()

type instance DispatchOf AppleScriptDynamic = Dynamic

-- | @since 0.1
notify :: (AppleScriptDynamic :> es) => Text -> Eff es ()
notify = send . Notify

runAppleScriptDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (AppleScriptDynamic : es) a ->
  Eff es a
runAppleScriptDynamicIO = reinterpret P.runTypedProcess $ \_ -> \case
  Notify t ->
    void
      . P.runProcess
      . P.shell
      . T.unpack
      $ t

notifyAppleScript :: (AppleScriptDynamic :> es) => ShrunNote -> Eff es ()
notifyAppleScript = notify . shrunToAppleScript

shrunToAppleScript :: ShrunNote -> Text
shrunToAppleScript shrunNote = txt
  where
    txt =
      mconcat
        [ "osascript -e 'display notification ",
          withDoubleQuotes (shrunNote ^. #body),
          " with title \"Shrun\" ",
          " subtitle ",
          withDoubleQuotes (shrunNote ^. #summary),
          "'"
        ]

withDoubleQuotes :: Text -> Text
withDoubleQuotes s = " \"" <> s <> "\" "
