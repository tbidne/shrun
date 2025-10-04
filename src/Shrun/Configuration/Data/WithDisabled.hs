module Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (..),

    -- * Construction
    fromMaybe,
    fromBool,

    -- * Elimination
    toMaybe,
    toBool,
    fromWithDisabled,
    fromDefault,
    fromMonoid,

    -- * Operators
    -- $operators
    (<>?),
    (<.>?),
    (<?>?),

    -- * Optics
    _With,
    _Without,
    _Disabled,
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude hiding (fromMaybe)

-- | Like Maybe but adds an extra constructor representing a "disabled" state.
-- The idea is that both CLI Args and Toml and have optional fields, but
-- the CLI can also be "disabled", which overrides everything.
--
-- The semigroup is similar to Maybe's:
--
-- - Identity: 'Without'
-- - 'With' is left-biased.
-- - ('Without', 'Disabled') forms a normal submonoid, in particular:
--
-- @
--   'Disabled' <> _ === 'Disabled' === _ <> 'Disabled'
-- @
data WithDisabled a
  = -- | The field.
    With a
  | -- | Missing.
    Without
  | -- | Disabled.
    Disabled
  deriving stock (Eq, Functor, Show)

_With :: Prism' (WithDisabled a) a
_With =
  prism
    With
    ( \case
        With x -> Right x
        y -> Left y
    )
{-# INLINE _With #-}

_Without :: Prism' (WithDisabled a) ()
_Without =
  prism
    (const Without)
    ( \case
        Without -> Right ()
        y -> Left y
    )
{-# INLINE _Without #-}

_Disabled :: Prism' (WithDisabled a) ()
_Disabled =
  prism
    (const Disabled)
    ( \case
        Disabled -> Right ()
        y -> Left y
    )
{-# INLINE _Disabled #-}

instance Foldable WithDisabled where
  foldr f e (With x) = f x e
  foldr _ e Without = e
  foldr _ e Disabled = e

instance Applicative WithDisabled where
  pure = With

  Disabled <*> _ = Disabled
  _ <*> Disabled = Disabled
  Without <*> _ = Without
  _ <*> Without = Without
  With f <*> With x = With (f x)

instance Monad WithDisabled where
  Disabled >>= _ = Disabled
  Without >>= _ = Without
  With x >>= f = f x

instance Semigroup (WithDisabled a) where
  Disabled <> _ = Disabled
  _ <> Disabled = Disabled
  Without <> r = r
  l <> _ = l

instance Monoid (WithDisabled a) where
  mempty = Without

instance Default (WithDisabled a) where
  def = Without

-- | 'With' -> 'Just', o/w -> 'Nothing'.
toMaybe :: WithDisabled a -> Maybe a
toMaybe (With x) = Just x
toMaybe _ = Nothing

toBool :: WithDisabled a -> Bool
toBool (With _) = True
toBool Without = False
toBool Disabled = False

-- | 'Nothing' -> 'Without', 'Just' -> 'With'.
fromMaybe :: Maybe a -> WithDisabled a
fromMaybe (Just x) = With x
fromMaybe Nothing = Without

fromBool :: Bool -> WithDisabled ()
fromBool True = With ()
fromBool False = Without

-- | Eliminates 'WithDisabled'.
fromWithDisabled :: a -> WithDisabled a -> a
fromWithDisabled _ (With y) = y
fromWithDisabled x _ = x

-- | Eliminates 'WithDisabled' via its 'Default' instance.
fromDefault :: (Default a) => WithDisabled a -> a
fromDefault = fromWithDisabled def

fromMonoid :: (Monoid a) => WithDisabled a -> a
fromMonoid = fromWithDisabled mempty

-- $operators
--
-- The operators follow a pattern. The basic idea is that we are running
-- the semigroup instance and doing something else, hence the base operator
-- is (<>).
--
-- A (?) on the left (right) side of the (<>) signifies that the LHS (RHS)
-- is a 'Maybe' that is lifted into 'WithDisabled', before running the
-- semigroup.
--
-- The symbol __inside__ of the (<>) indicates the result. A dot
-- i.e. (<.>) is meant to evoke field extraction aka record-dot-syntax.
-- In other words, this operator runs the semigroup then extracts the
-- underlying field, falling back to the 'Default' instance.
--
-- The (?) symbol, on the other hand, indicates the result is a 'Maybe'.

-- | @l <>? r@ lifts 'Maybe' @r@ into a 'WithDisabled' per 'fromMaybe' then
-- runs the 'Semigroup'.
(<>?) :: WithDisabled a -> Maybe a -> WithDisabled a
wd <>? m = wd <> fromMaybe m

infixr 6 <>?

-- | Like '(<>?)' except we extract a result via 'fromDefault'.
(<.>?) :: (Default a) => WithDisabled a -> Maybe a -> a
x <.>? y = fromDefault (x <>? y)

infixr 6 <.>?

-- | Like '(<>?)' except we extract a Maybe via 'toMaybe'.
(<?>?) :: WithDisabled a -> Maybe a -> Maybe a
x <?>? y = toMaybe (x <>? y)

infixr 6 <?>?
