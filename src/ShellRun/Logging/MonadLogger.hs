-- | Provides the `MonadLogger` class, used for having pretty logs in
-- a monadic setting. Logging via this class includes both textual
-- prefixes (e.g. @[INFO]@, @[ERROR]@) and also common terminal control
-- prefixes for colors.
module ShellRun.Logging.MonadLogger
  ( -- * Typeclass for logging
    MonadLogger (..),

    -- * Pretty formatted logging
    putLogNone,
    putLogSubCommand,
    putLogDebug,
    putLogInfo,
    putLogInfoBlue,
    putLogInfoCyan,
    putLogInfoSuccess,
    putLogWarn,
    putLogError,
    putLogFatal,
  )
where

import Control.Monad.Reader qualified as MTL
import Data.Text qualified as T
import ShellRun.Logging.Log (Log (..))
import ShellRun.Logging.Log qualified as Log
import ShellRun.Prelude
import System.Console.Pretty qualified as P
import System.Console.Terminal.Size (Window (..))
import System.Console.Terminal.Size qualified as TermSz
import System.IO qualified as IO

-- | `MonadLogger` is a simple typeclass for abstracting logging functions.
type MonadLogger :: (Type -> Type) -> Constraint
class Monad m => MonadLogger m where
  putLog :: Log -> m ()
  clear :: m ()

instance MonadLogger IO where
  putLog :: Log -> IO ()
  putLog lg@MkLog {msg, mode} = do
    let color = Log.logToColor lg
        prefix = Log.logToPrefix lg
        (logFn, msg') = case mode of
          Log.NewLine -> (logLine, msg)
          Log.CarriageReturn -> (logNoLine, msg <> "\r")
          Log.Plain -> (logNoLine, msg)

    clear
    logFn $ P.color color $ prefix <> msg'
    where
      logLine = putStrLn
      logNoLine txt = putStr txt *> IO.hFlush IO.stdout

  clear :: IO ()
  clear = do
    -- Clear the entire term, fallback to 80 if we cannot get the width.
    spaces <-
      TermSz.size <&> \case
        Nothing -> T.pack $ replicate 80 ' '
        Just Window {width} -> T.pack $ replicate width ' '

    putStr $ "\r" <> spaces <> "\r"

instance MonadLogger m => MonadLogger (ReaderT e m) where
  putLog :: Log -> ReaderT e m ()
  putLog = MTL.lift . putLog

  clear :: ReaderT e m ()
  clear = MTL.lift clear

-- | 'putLog' with 'Log.logNone'.
putLogNone :: MonadLogger m => Text -> m ()
putLogNone = putLog . Log.logNone

-- | 'putLog' with 'Log.logSubCommand'.
putLogSubCommand :: MonadLogger m => Text -> m ()
putLogSubCommand = putLog . Log.logSubCommand

-- | 'putLog' with 'Log.logDebug'.
putLogDebug :: MonadLogger m => Text -> m ()
putLogDebug = putLog . Log.logDebug

-- | 'putLog' with 'Log.logInfo'.
putLogInfo :: MonadLogger m => Text -> m ()
putLogInfo = putLog . Log.logInfo

-- | 'putLog' with 'Log.logInfoBlue'.
putLogInfoBlue :: MonadLogger m => Text -> m ()
putLogInfoBlue = putLog . Log.logInfoBlue

-- | 'putLog' with 'Log.logInfoCyan'.
putLogInfoCyan :: MonadLogger m => Text -> m ()
putLogInfoCyan = putLog . Log.logInfoCyan

-- | 'putLog' with 'Log.logInfoSuccess'.
putLogInfoSuccess :: MonadLogger m => Text -> m ()
putLogInfoSuccess = putLog . Log.logInfoSuccess

-- | 'putLog' with 'Log.logWarn'.
putLogWarn :: MonadLogger m => Text -> m ()
putLogWarn = putLog . Log.logWarn

-- | 'putLog' with 'Log.logError'.
putLogError :: MonadLogger m => Text -> m ()
putLogError = putLog . Log.logError

-- | 'putLog' with 'Log.logFatal'.
putLogFatal :: MonadLogger m => Text -> m ()
putLogFatal = putLog . Log.logFatal
