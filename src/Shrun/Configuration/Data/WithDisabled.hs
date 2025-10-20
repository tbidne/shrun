module Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (..),
    disabledParser,
    toMaybe,
    (<|?|>),
  )
where

import Shrun.Prelude hiding (fromMaybe)

-- | WithDisabled augments some type with a 'disabled' option. Isomorphic to
-- 'Maybe', it exists to distinguish "missing" vs. explicitly
-- disabled. For instance, we want to be able to disable the parameter
-- notify-action, but we do not want to add another constructor to it, because
-- we want NotifyAction to represent definite actions, hence the user config
-- (CLI and Toml) is 'Maybe (Disabled NotifyAction)' i.e.
--
-- - Just Disabled: NotifyAction explicitly disabled.
-- - Nothing: NotifyAction not given.
-- - Just Enabled _: NotifyAction explicity enabled.
--
-- We have this for the purposes of allowing CLI the override whatever
-- Toml parameter might exist, while at shrun's runtime it is merged to
-- 'Maybe NotifyAction':
--
-- - Nothing: Notifications off.
-- - Just NotifyAction: Notifications on.
data WithDisabled a
  = -- | The field.
    With a
  | -- | Disabled.
    Disabled
  deriving stock (Eq, Functor, Show)

instance (DecodeTOML a) => DecodeTOML (WithDisabled a) where
  tomlDecoder = parseText <|> With <$> tomlDecoder
    where
      parseText = do
        tomlDecoder @Text >>= \case
          "off" -> pure Disabled
          other -> fail $ "Expected 'off', received: " <> unpack other

disabledParser :: (Applicative f) => Text -> f a -> f (WithDisabled a)
disabledParser "off" _ = pure Disabled
disabledParser _ fx = With <$> fx

-- | Runs Maybes Alternative instance, then binds with toMaybe. I.e., takes
-- any Just if it exists (left-biased), then performs monadic join.
(<|?|>) :: Maybe (WithDisabled a) -> Maybe (WithDisabled a) -> Maybe a
mx <|?|> my = (mx <|> my) >>= toMaybe

infixr 6 <|?|>

toMaybe :: WithDisabled a -> Maybe a
toMaybe (With a) = Just a
toMaybe Disabled = Nothing
