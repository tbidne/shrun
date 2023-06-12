-- | Provides types for typical "IO" processes.
module Shrun.IO.Types
  ( Stderr (..),
    CommandResult (..),
    ReadHandleResult (..),
    readHandleResultToStderr,
    readHandle,
  )
where

import Data.Text qualified as T
import Data.Time.Relative (RelativeTime)
import Effects.FileSystem.HandleReader
  ( MonadHandleReader (hIsClosed),
    hGetNonBlocking,
    hIsReadable,
  )
import Shrun.Prelude

-- | Newtype wrapper for stderr.
newtype Stderr = MkStderr {getStderr :: Text}
  deriving stock (Eq, Show)

-- | Result of running a command.
data CommandResult
  = CommandSuccess !RelativeTime
  | CommandFailure !RelativeTime !Stderr
  deriving stock (Eq, Show)

-- | Result from reading a handle. The ordering is based on:
--
-- @
-- 'ReadErr' _ < 'ReadNoData' < 'ReadSuccess'
-- @
--
-- The 'Semigroup' instance is based on this ordering, taking the greatest
-- element. For identical constructors, the left argument is taken.
data ReadHandleResult
  = -- | Error encountered while trying to read a handle.
    ReadErr !Text
  | -- | Successfully read data from the handle.
    ReadSuccess ![Text]
  | -- | Successfully read no data from the handle.
    ReadNoData
  deriving stock (Eq, Show)

instance Semigroup ReadHandleResult where
  ReadErr l <> _ = ReadErr l
  _ <> ReadErr r = ReadErr r
  ReadSuccess l <> _ = ReadSuccess l
  _ <> ReadSuccess r = ReadSuccess r
  _ <> _ = ReadNoData

-- | Turns a 'ReadHandleResult' into a 'Stderr'.
readHandleResultToStderr :: ReadHandleResult -> Stderr
readHandleResultToStderr ReadNoData = MkStderr "<No data>"
readHandleResultToStderr (ReadErr err) = MkStderr err
readHandleResultToStderr (ReadSuccess err) = MkStderr (fold err)

-- | Attempts to read from the handle.
readHandle :: (MonadCatch m, MonadHandleReader m) => Handle -> m ReadHandleResult
readHandle handle = do
  -- The "nothingIfReady" check and reading step both need to go in the try as
  -- the former can also throw.
  tryAny readHandle' <&> \case
    Left ex -> ReadErr $ "HandleException: " <> displayExceptiont ex
    Right x -> x
  where
    readHandle' =
      nothingIfReady >>= \case
        Just err -> pure $ ReadErr err
        Nothing ->
          -- NOTE: [Blocking / Streaming output]
          --
          -- hGetNonBlocking is suboptimal because we might take
          -- multiple lines of output, hence log them on a single line, which
          -- is quite ugly. Something like hGetLine would be ideal, but try
          -- as we might we cannot get this to stream properly (logs are
          -- buffered until the process finishes). Thus we have settled on
          -- a workaround: Use the following non blocking call which streams
          -- properly, and manually split the lines ourselves. The block size
          -- should be large enough that we are not likely to cut off a line
          -- prematurely, but obviously this is best-effort. We can make this
          -- configurable should the need arise.
          hGetNonBlocking handle blockSize <&> \case
            "" -> ReadNoData
            bs -> ReadSuccess (T.lines $ decodeUtf8Lenient bs)

    nothingIfReady = do
      -- NOTE: This somewhat torturous logic exists for a reason. We want to
      -- check several conditions before attempting to read from our handle,
      -- but we have to do this in a specific order as some of these boolean
      -- functions will throw exceptions under some circumstances, which we
      -- would like to avoid.
      --
      -- Note that this description comes from experience and reading the
      -- GHC source, so it may not be completely accurate.

      -- hIsClosed does not explicitly throw exceptions so it can be first.
      isClosed <- hIsClosed handle
      if isClosed
        then pure $ Just "Handle closed"
        else do
          -- hIsReadable _does_ throw an exception if the the handle is closed or
          -- "semi-closed". Thus it should go after the hIsClosed check
          -- (GHC explicitly does not export an hSemiClosed).
          isReadable <- hIsReadable handle
          if not isReadable
            then pure $ Just "Handle is not readable"
            else pure Nothing

-- NOTE: [EOF / blocking error] We would like to check hIsEOF (definitely
-- causes errors at the end) and probably hReady as well, but these both
-- block and I have not found a way to invoke them while also streaming
-- the process output (blocks until everything gets dumped at the end).

blockSize :: Int
blockSize = 1024
