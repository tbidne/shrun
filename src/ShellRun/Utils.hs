-- | Provides utilities.
module ShellRun.Utils
  ( -- * Text Utils
    module TextUtils,

    -- * Timing Utils
    diffTime,

    -- * Misc Utils
    displayCommand,

    -- * Math Utils
    divWithRem,
  )
where

import Refined (NonNegative, Positive, Refined)
import Refined qualified as R
import Refined.Unsafe qualified as R
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
import ShellRun.Prelude
import ShellRun.Utils.Text as TextUtils
import System.Clock (TimeSpec (..))
import System.Clock qualified as C

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Refined qualified as R

-- | For \(n \ge 0, d > 0\), @divWithRem n d@ returns non-negative \((e, r)\) such that
--
-- \[
--    \begin{align}
--      de + r = n \\
--      r \le n \\
--    \end{align}
-- \]
--
-- Examples:
--
-- >>> :{
--   let n = $$(R.refineTH @NonNegative @Int 34)
--       d = $$(R.refineTH @Positive @Int 5)
--       result = divWithRem n d
--   in monoBimap R.unrefine result
-- :}
-- (6,4)
--
-- >>> :{
--   let n = $$(R.refineTH @NonNegative @Int 12)
--       d = $$(R.refineTH @Positive @Int 18)
--       result = divWithRem n d
--   in monoBimap R.unrefine result
-- :}
-- (0,12)
divWithRem ::
  Refined NonNegative Int ->
  Refined Positive Int ->
  (Refined NonNegative Int, Refined NonNegative Int)
divWithRem n d = monoBimap R.unsafeRefine (n' `div` d', n' `rem` d')
  where
    n' = R.unrefine n
    d' = R.unrefine d

-- | For given \(x, y\), returns the absolute difference \(|x - y|\)
-- in seconds.
--
-- >>> :{
--   let t1 = TimeSpec 5 0
--       -- 20 s + 1 billion ns = 21 s
--       t2 = TimeSpec 20 1_000_000_000
--   in diffTime t1 t2
-- :}
-- Refined 16
diffTime :: TimeSpec -> TimeSpec -> Refined NonNegative Int
diffTime t1 t2 =
  let diff = fromIntegral $ C.sec $ C.diffTimeSpec t1 t2
   in -- Safe because 'C.diffTimeSpec' guaranteed to be non-negative.
      R.unsafeRefine diff

-- | Returns the key if one exists and we pass in 'ShowKey', otherwise
-- returns the command.
--
-- >>> displayCommand ShowKey (MkCommand Nothing "cmd")
-- "cmd"
--
-- >>> displayCommand ShowCommand (MkCommand (Just "key") "cmd")
-- "cmd"
--
-- >>> displayCommand ShowKey (MkCommand (Just "key") "cmd")
-- "key"
displayCommand :: CommandDisplay -> Command -> Text
displayCommand ShowKey (MkCommand (Just key) _) = key
displayCommand _ (MkCommand _ cmd) = cmd
