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
-- we want NotifyActionComplete to represent definite actions, hence the user config
-- (CLI and Toml) is 'Maybe (Disabled NotifyActionComplete)' i.e.
--
-- - Just Disabled: NotifyActionComplete explicitly disabled.
-- - Nothing: NotifyActionComplete not given.
-- - Just Enabled _: NotifyActionComplete explicity enabled.
--
-- We have this for the purposes of allowing CLI the override whatever
-- Toml parameter might exist, while at shrun's runtime it is merged to
-- 'Maybe NotifyActionComplete':
--
-- - Nothing: Notifications off.
-- - Just NotifyActionComplete: Notifications on.
data WithDisabled a
  = -- | The field.
    With a
  | -- | Disabled.
    Disabled
  deriving stock (Eq, Functor, Show)

instance Applicative WithDisabled where
  pure = With

  Disabled <*> _ = Disabled
  _ <*> Disabled = Disabled
  With f <*> With x = With (f x)

-- We have an Alternative instance because we want one like Maybe that is
-- left-biased. We leave Semigroup/Monoid alone for now as there is no
-- need unless we want to also use the type variable in some way.

instance Alternative WithDisabled where
  empty = Disabled

  With x <|> _ = With x
  Disabled <|> y = y

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
