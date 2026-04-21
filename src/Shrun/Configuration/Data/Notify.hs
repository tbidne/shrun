{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Shrun.Configuration.Data.Notify
  ( -- * NotifyInit
    NotifyActionsInit (..),

    -- * NotifyActive
    NotifyActionsActive (..),
    _NotifyActionsActiveCompleteAny,
    _NotifyActionsActiveStartAny,

    -- * Notify
    NotifyP (..),
    NotifyArgs,
    NotifyToml,
    NotifyMerged,
    NotificationEnv,
    mergeNotifications,
    toEnv,
  )
where

import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
    SwitchF,
  )
import Shrun.Configuration.Data.Notify.Action
  ( NotifyActionComplete,
    NotifyActionStartSwitch,
  )
import Shrun.Configuration.Data.Notify.System
  ( mergeNotifySystem,
    parseNotifySystem,
  )
import Shrun.Configuration.Data.Notify.Timeout
  ( notifyTimeoutDecoder,
    prettyNotifyTimeout,
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With),
    (<|?|>),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Configuration.Default (Default, def, (<.>))
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- See NOTE: [Args vs. Toml mandatory fields]

-- | Notify action is optional on Init since all of this is optional.
-- We do not need to specify any other phases since we switch to
-- NotifyActionsActive.
type NotifyActionCompleteF :: ConfigPhase -> Type
type family NotifyActionCompleteF p where
  NotifyActionCompleteF ConfigPhaseArgs = Maybe (WithDisabled NotifyActionComplete)
  NotifyActionCompleteF ConfigPhaseToml = Maybe (WithDisabled NotifyActionComplete)

-- | Initial notify action config.
data NotifyActionsInit p = MkNotifyActionsInit
  { complete :: NotifyActionCompleteF p,
    start :: SwitchF p NotifyActionStartSwitch
  }

instance
  ( k ~ A_Lens,
    a ~ NotifyActionCompleteF p,
    b ~ NotifyActionCompleteF p
  ) =>
  LabelOptic "complete" k (NotifyActionsInit p) (NotifyActionsInit p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkNotifyActionsInit a1 a2) ->
        fmap
          (\b -> MkNotifyActionsInit b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ SwitchF p NotifyActionStartSwitch,
    b ~ SwitchF p NotifyActionStartSwitch
  ) =>
  LabelOptic "start" k (NotifyActionsInit p) (NotifyActionsInit p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkNotifyActionsInit a1 a2) ->
        fmap
          (\b -> MkNotifyActionsInit a1 b)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( NotifyActionCompleteF p ~ Maybe (WithDisabled NotifyActionComplete),
    SwitchF p NotifyActionStartSwitch ~ Maybe NotifyActionStartSwitch
  ) =>
  Semigroup (NotifyActionsInit p)
  where
  l <> r =
    MkNotifyActionsInit
      { complete = l ^. #complete <|> r ^. #complete,
        start = l ^. #start <|> r ^. #start
      }

instance
  ( NotifyActionCompleteF p ~ Maybe (WithDisabled NotifyActionComplete),
    SwitchF p NotifyActionStartSwitch ~ Maybe NotifyActionStartSwitch
  ) =>
  Monoid (NotifyActionsInit p)
  where
  mempty =
    MkNotifyActionsInit
      { complete = Nothing,
        start = Nothing
      }

deriving stock instance Eq (NotifyActionsInit ConfigPhaseArgs)

deriving stock instance Show (NotifyActionsInit ConfigPhaseToml)

deriving stock instance Eq (NotifyActionsInit ConfigPhaseToml)

deriving stock instance Show (NotifyActionsInit ConfigPhaseArgs)

-- | Holds active notif actions. This is morally 'These', specialized
-- to our notify action use case.
data NotifyActionsActive
  = -- | Complete actions active.
    NotifyActionsActiveComplete NotifyActionComplete
  | -- | Start actions active.
    NotifyActionsActiveStart
  | -- | All actions active.
    NotifyActionsActiveAll NotifyActionComplete
  deriving stock (Eq, Show)

instance Pretty NotifyActionsActive where
  pretty = \case
    NotifyActionsActiveComplete c ->
      vcat
        [ "action-complete: " <> pretty c,
          "action-start: off"
        ]
    NotifyActionsActiveStart ->
      vcat
        [ "action-complete: off",
          "action-start: on"
        ]
    NotifyActionsActiveAll c ->
      vcat
        [ "action-complete: " <> pretty c,
          "action-start: on"
        ]

-- NOTE:
--
-- These optics do not exactly match the constructors because they are intended
-- to match several. I have not checked if this is lawful, but they are
-- useful, so whatever.

_NotifyActionsActiveCompleteAny :: AffineFold NotifyActionsActive NotifyActionComplete
_NotifyActionsActiveCompleteAny = afolding $ \case
  NotifyActionsActiveComplete x -> Just x
  NotifyActionsActiveStart -> Nothing
  NotifyActionsActiveAll x -> Just x
{-# INLINE _NotifyActionsActiveCompleteAny #-}

_NotifyActionsActiveStartAny :: AffineFold NotifyActionsActive ()
_NotifyActionsActiveStartAny = afolding $ \case
  NotifyActionsActiveComplete _ -> Nothing
  NotifyActionsActiveStart -> Just ()
  NotifyActionsActiveAll _ -> Just ()
{-# INLINE _NotifyActionsActiveStartAny #-}

type NotifyActionsF :: ConfigPhase -> Type
type family NotifyActionsF p where
  NotifyActionsF ConfigPhaseArgs = NotifyActionsInit ConfigPhaseArgs
  NotifyActionsF ConfigPhaseToml = NotifyActionsInit ConfigPhaseToml
  NotifyActionsF ConfigPhaseMerged = NotifyActionsActive
  NotifyActionsF ConfigPhaseEnv = NotifyActionsActive

type NotifySystemF :: ConfigPhase -> Type
type family NotifySystemF p where
  NotifySystemF ConfigPhaseArgs = Maybe NotifySystem
  NotifySystemF ConfigPhaseToml = Maybe NotifySystem
  NotifySystemF ConfigPhaseMerged = NotifySystem
  NotifySystemF ConfigPhaseEnv = NotifyEnv

-- | Holds notification config. We have an invariant that if the notify config
-- exists (i.e. is 'Just'), then at least one of actionComplete, actionStart
-- should be Just/True.
type NotifyP :: ConfigPhase -> Type
data NotifyP p = MkNotifyP
  { -- | Notify actions.
    actions :: NotifyActionsF p,
    -- | The notification system to use.
    system :: NotifySystemF p,
    -- | when to timeout successful notifications.
    timeout :: ConfigPhaseF p NotifyTimeout
  }

instance
  ( k ~ A_Lens,
    a ~ NotifyActionsF p,
    b ~ NotifyActionsF p
  ) =>
  LabelOptic "actions" k (NotifyP p) (NotifyP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkNotifyP a1 a2 a3) ->
        fmap
          (\b -> MkNotifyP b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ NotifySystemF p,
    b ~ NotifySystemF p
  ) =>
  LabelOptic "system" k (NotifyP p) (NotifyP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkNotifyP a1 a2 a3) ->
        fmap
          (\b -> MkNotifyP a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p NotifyTimeout,
    b ~ ConfigPhaseF p NotifyTimeout
  ) =>
  LabelOptic "timeout" k (NotifyP p) (NotifyP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkNotifyP a1 a2 a3) ->
        fmap
          (\b -> MkNotifyP a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

instance Semigroup NotifyToml where
  l <> r =
    MkNotifyP
      { actions = l ^. #actions <> r ^. #actions,
        system = l ^. #system <|> r ^. #system,
        timeout = l ^. #timeout <|> r ^. #timeout
      }

instance Monoid NotifyToml where
  mempty =
    MkNotifyP
      { actions = mempty,
        system = Nothing,
        timeout = Nothing
      }

instance Pretty NotifyMerged where
  pretty c =
    vcat
      [ pretty (c ^. #actions),
        "system: " <> pretty (display $ c ^. #system),
        "timeout: " <> prettyNotifyTimeout (c ^. #timeout)
      ]

type NotifyArgs = NotifyP ConfigPhaseArgs

type NotifyToml = NotifyP ConfigPhaseToml

type NotifyMerged = NotifyP ConfigPhaseMerged

-- Named NotificationEnv vs NotifyEnv to avoid clash with effects-notify.
type NotificationEnv = NotifyP ConfigPhaseEnv

deriving stock instance Eq (NotifyP ConfigPhaseArgs)

deriving stock instance Show (NotifyP ConfigPhaseArgs)

deriving stock instance Eq (NotifyP ConfigPhaseToml)

deriving stock instance Show (NotifyP ConfigPhaseToml)

deriving stock instance Eq (NotifyP ConfigPhaseMerged)

deriving stock instance Show (NotifyP ConfigPhaseMerged)

-- Only Default instance is for Args, since others require the action.
instance Default NotifyArgs where
  def =
    MkNotifyP
      { actions = mempty,
        system = Nothing,
        timeout = Nothing
      }

-- | Merges args and toml configs.
mergeNotifications ::
  NotifyArgs ->
  Maybe NotifyToml ->
  Maybe NotifyMerged
mergeNotifications args mToml = do
  case mActions of
    Nothing -> Nothing
    Just actions -> do
      -- Also, no need to do this unless we are actually running with
      -- notifications.
      let system = mergeNotifySystem (args ^. #system) (mToml ^? Utils.surroundJust #system)
      Just
        $ MkNotifyP
          { actions,
            system,
            timeout =
              (args ^. #timeout) <.> (mToml ^? Utils.surroundJust #timeout)
          }
  where
    mActionComplete :: Maybe NotifyActionComplete
    mActionComplete =
      args ^. #actions % #complete <|?|> (mToml ^? Utils.surroundJust (#actions % #complete))

    mActionStart :: NotifyActionStartSwitch
    mActionStart =
      args ^. #actions % #start <.> (mToml ^? Utils.surroundJust (#actions % #start))

    mActions = case (mActionComplete, mActionStart ^. #unNotifyActionStartSwitch) of
      (Nothing, False) -> Nothing
      (Just actionComplete, False) -> Just $ NotifyActionsActiveComplete actionComplete
      (Nothing, True) -> Just NotifyActionsActiveStart
      (Just actionComplete, True) -> Just $ NotifyActionsActiveAll actionComplete

instance DecodeTOML NotifyToml where
  tomlDecoder = do
    complete <- getFieldOptWith tomlDecoder "action-complete"
    start <- getFieldOptWith tomlDecoder "action-start"

    let actions =
          MkNotifyActionsInit
            { complete,
              start
            }

    system <- getFieldOptWith (parseNotifySystem tomlDecoder) "system"
    timeout <- getFieldOptWith notifyTimeoutDecoder "timeout"
    pure
      $ MkNotifyP
        { actions,
          system,
          timeout
        }

toEnv ::
  ( HasCallStack,
    MonadNotify m,
    MonadThrow m
  ) =>
  NotifyMerged ->
  m (NotifyP ConfigPhaseEnv)
toEnv notifyMerged = do
  system <- notifySystemToOs systemMerged
  notifyEnv <- initNotifyEnv system
  pure $ mkNotify notifyMerged notifyEnv
  where
    systemMerged = notifyMerged ^. #system
{-# INLINEABLE toEnv #-}

mkNotify :: NotifyMerged -> NotifySystemF ConfigPhaseEnv -> NotifyP ConfigPhaseEnv
mkNotify notifyToml systemP2 =
  MkNotifyP
    { actions = notifyToml ^. #actions,
      system = systemP2,
      timeout = notifyToml ^. #timeout
    }
