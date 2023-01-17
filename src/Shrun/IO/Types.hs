{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types for typical "IO" processes.
--
-- @since X-X-X
module Shrun.IO.Types
  ( Stdout (..),
    Stderr (..),
    makeStdErr,
    ReadHandleResult (..),
    readHandleResultToStderr,
    readHandle,
  )
where

import Data.Text qualified as T
import Effects.FileSystem.MonadHandleReader (MonadHandleReader (hIsClosed), hGetNonBlocking, hIsReadable)
import Shrun.Data.Supremum (Supremum (..))
import Shrun.Prelude

-- | Newtype wrapper for stdout.
--
-- @since 0.1
newtype Stdout = MkStdout
  { -- | @since 0.1
    getStdout :: Text
  }

-- | @since 0.7
makeFieldLabelsNoPrefix ''Stdout

-- | Newtype wrapper for stderr.
--
-- @since 0.1
newtype Stderr = MkStderr
  { -- | @since 0.1
    getStderr :: Text
  }

-- | @since 0.7
makeFieldLabelsNoPrefix ''Stderr

-- | @since 0.1
makeStdErr :: Text -> Stderr
makeStdErr err = MkStderr $ "Error: '" <> T.strip err
{-# INLINE makeStdErr #-}

-- | Result from reading a handle. The ordering is based on:
--
-- @
-- 'ReadErr' _ < 'ReadNoData' < 'ReadSuccess'
-- @
--
-- The 'Semigroup' instance is based on this ordering, taking the greatest
-- element. For identical constructors, the left argument is taken.
--
-- @since 0.1
data ReadHandleResult
  = -- | Error encountered while trying to read a handle.
    --
    -- @since 0.1
    ReadErr Text
  | -- | Successfully read data from the handle.
    --
    -- @since 0.1
    ReadSuccess Text
  | -- | Successfully read no data from the handle.
    --
    -- @since 0.1
    ReadNoData
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via Supremum ReadHandleResult

-- | @since 0.1
instance Bounded ReadHandleResult where
  minBound = ReadErr ""
  {-# INLINEABLE minBound #-}
  maxBound = ReadSuccess ""
  {-# INLINEABLE maxBound #-}

-- | @since 0.1
instance Ord ReadHandleResult where
  compare x y | x == y = EQ
  compare (ReadSuccess _) _ = GT
  compare _ (ReadSuccess _) = LT
  compare ReadNoData _ = GT
  compare _ ReadNoData = LT
  compare (ReadErr _) _ = GT
  {-# INLINEABLE compare #-}

-- | Turns a 'ReadHandleResult' into a 'Stderr'.
--
-- @since 0.1
readHandleResultToStderr :: ReadHandleResult -> Stderr
readHandleResultToStderr ReadNoData = MkStderr "<No data>"
readHandleResultToStderr (ReadErr err) = MkStderr err
readHandleResultToStderr (ReadSuccess err) = MkStderr err
{-# INLINEABLE readHandleResultToStderr #-}

-- | Attempts to read from the handle.
--
-- @since 0.1
readHandle :: (MonadCatch m, MonadHandleReader m) => Handle -> m ReadHandleResult
readHandle handle = do
  let displayEx :: Show a => Text -> a -> Text
      displayEx prefix =
        view #getStderr
          . makeStdErr
          . (<>) prefix
          . showt
      readEx = displayEx "Handle exception: "

  (isClosed, canRead) <-
    (,)
      <$> hIsClosed handle
      <*> hIsReadable handle
  if
      | isClosed ->
          pure $ ReadErr $ displayEx @String "Handle closed" ""
      | not canRead ->
          pure $ ReadErr $ displayEx @String "Cannot read from handle" ""
      | otherwise -> do
          output :: Either SomeException ByteString <-
            tryAny $ hGetNonBlocking handle blockSize
          let outDecoded = fmap decodeUtf8Lenient output
          case outDecoded of
            Left ex -> pure $ ReadErr $ readEx ex
            Right "" -> pure ReadNoData
            Right o -> pure $ ReadSuccess o
{-# INLINEABLE readHandle #-}

blockSize :: Int
blockSize = 1024
{-# INLINEABLE blockSize #-}
