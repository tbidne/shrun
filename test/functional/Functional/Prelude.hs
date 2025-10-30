{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

-- TODO:
--
--   1. It would be nice if we could test that we do not receive any "extra"
--      output e.g. "ExitFailure 1". To do this, though, we'd have to test
--      with the exception logic, since the exception stuff happens _outside_
--      of these tests i.e. exceptions are caught and the unwanted output will
--      occur from the handler (set in Main.hs).
--
--  42. Consider testing --help (would require some refactoring like 3 above).

module Functional.Prelude
  ( module X,

    -- * Running tests
    run,
    runNotes,
    runException,
    runExceptionE,
    runExitFailure,
    runAllExitFailure,
    runCancelled,

    -- ** Mocked IO (Configuration)
    FuncIOEnv (..),
    runFuncIO,

    -- ** Read strategies
    ReadStrategyTestParams (..),
    ReadStrategyTest.testReadStrategy,
    ReadStrategyTest.multiTestReadStrategy,

    -- * Expectations

    -- ** Text
    debugPrefix,
    warnPrefix,
    errorPrefix,
    fatalPrefix,
    commandPrefix,
    killedPrefix,
    timerPrefix,
    finishedPrefix,
    waitingPrefix,
    runningPrefix,
    timedOut,

    -- ** Prefixes
    withCommandPrefix,
    withDebugPrefix,
    withSuccessPrefix,
    withErrorPrefix,
    withWarnPrefix,
    withFatalPrefix,
    withTimerPrefix,
    withFinishedPrefix,
    withKilledPrefix,

    -- * Misc
    withBaseArgs,
    withNoConfig,
    appendScriptsHome,
    scriptsHomeStr,
    notifySystemArg,
    readLogFile,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Data.Typeable (typeRep)
import Effects.Concurrent.Async qualified as Async
import FileSystem.OsPath as X (combineFilePaths, unsafeDecode)
import Functional.Prelude.FuncEnv
  ( FuncEnv (MkFuncEnv, coreEnv, funcIOEnv, logs, shrunNotes),
    FuncIOEnv (MkFuncIOEnv, cwdDir, xdgDir),
    unFuncIO,
  )
import Functional.ReadStrategyTest
  ( ReadStrategyTestParams
      ( ReadStrategyTestParametricSetup,
        ReadStrategyTestParametricSimple,
        ReadStrategyTestSimple
      ),
  )
import Functional.ReadStrategyTest qualified as ReadStrategyTest
import Shrun qualified as SR
import Shrun.Configuration.Env qualified as Env
import Shrun.Notify.DBus (MonadDBus)
import Shrun.Notify.MonadNotify (ShrunNote)
import Shrun.Prelude as X
import Test.Shrun.Verifier (ResultText (MkResultText))
import Test.Tasty as X
  ( TestTree,
    askOption,
    defaultMain,
    testGroup,
    withResource,
  )
import Test.Tasty.HUnit as X
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )

-- | Runs the args and retrieves the logs.
run :: List String -> IO (List ResultText)
run = fmap fst . runMaybeException ExNothing

-- | Like 'run', but with 'FuncIO' instead of normal 'IO'.
runFuncIO :: FuncIOEnv -> List String -> IO (List ResultText)
runFuncIO env =
  usingReaderT env
    . unFuncIO
    . fmap fst
    . runMaybeException ExNothing

-- | Runs the args and retrieves the sent notifications.
runNotes :: List String -> IO (List ShrunNote)
runNotes = fmap snd . runMaybeException ExNothing

-- | 'runException' specialized to ExitFailure.
runExitFailure :: List String -> IO (List ResultText)
runExitFailure =
  fmap fst
    . runMaybeException (ExJust $ Proxy @ExitCode)

runAllExitFailure :: List String -> IO (Tuple2 (List ResultText) (List ShrunNote))
runAllExitFailure = runMaybeException (ExJust $ Proxy @ExitCode)

-- | Like 'runException', except it expects an exception.
runException ::
  forall e.
  (Exception e) =>
  List String ->
  IO (List ResultText)
runException = fmap fst . runMaybeException (ExJust (Proxy @e))

-- | So we can hide the exception type and make it so run does not
-- have to pass in a dummy var to runMaybeException.
data MaybeException where
  ExNothing :: MaybeException
  ExJust :: (Exception e) => Proxy e -> MaybeException

-- | Runs shrun potentially catching an expected exception.
runMaybeException ::
  forall m.
  (ShrunCons m) =>
  MaybeException ->
  List String ->
  m (List ResultText, List ShrunNote)
runMaybeException mException argList = do
  ls <- newIORef []
  shrunNotes <- newIORef []

  let action = do
        withArgs argList $ Env.withEnv $ \env -> do
          let funcEnv =
                MkFuncEnv
                  { coreEnv = env,
                    funcIOEnv = MkFuncIOEnv Nothing Nothing,
                    logs = ls,
                    shrunNotes
                  }

          SR.runShellT SR.shrun funcEnv

  case mException of
    -- 1. Not expecting an exception
    ExNothing -> do
      tryMySync action >>= \case
        -- 1.1: Received an exception: print logs and rethrow
        Left ex -> printLogsReThrow ex ls
        -- 1.2: No exception, return logs/notes
        Right _ -> readRefs ls shrunNotes
    -- 2. Expecting exception e
    ExJust @e proxy ->
      tryMySync action >>= \case
        -- 2.1: Received no exception: print logs and die
        Right _ -> do
          printLogs ls
          error
            $ mconcat
              [ "Expected exception <",
                show (typeRep proxy),
                ">, received none"
              ]
        Left someEx -> do
          case fromException @e someEx of
            -- 2.2: Received exception e: return logs/notes
            Just _ -> readRefs ls shrunNotes
            -- 2.3: Received some other exception: print logs and die
            Nothing -> do
              printLogs ls
              error
                $ mconcat
                  [ "Expected exception <",
                    show (typeRep proxy),
                    ">, but received another: ",
                    displayException someEx
                  ]
  where
    readRefs ::
      IORef (List Text) ->
      IORef (List ShrunNote) ->
      m (List ResultText, List ShrunNote)
    readRefs ls ns = ((,) . fmap MkResultText . L.reverse <$> readIORef ls) <*> readIORef ns

    printLogsReThrow :: (Exception e) => e -> IORef (List Text) -> m void
    printLogsReThrow ex ls = do
      printLogs ls

      -- rethrow
      throwM ex

    printLogs :: IORef (List Text) -> m ()
    printLogs ls = do
      logs <- readIORef ls

      putStrLn "\n*** LOGS ***\n"

      for_ logs (putStrLn . unpack)
      putStrLn ""

-- | Runs shrun potentially catching an expected exception.
runExceptionE ::
  forall e.
  (Exception e) =>
  List String ->
  IO (List ResultText, e)
runExceptionE argList = do
  ls <- newIORef []
  shrunNotes <- newIORef []

  let action = do
        withArgs argList $ Env.withEnv $ \env -> do
          let funcEnv =
                MkFuncEnv
                  { coreEnv = env,
                    funcIOEnv = MkFuncIOEnv Nothing Nothing,
                    logs = ls,
                    shrunNotes
                  }

          SR.runShellT SR.shrun funcEnv

  tryMySync action >>= \case
    -- 2.1: Received no exception: print logs and die
    Right _ -> do
      printLogs ls
      error
        $ mconcat
          [ "Expected exception <",
            show (typeRep @_ @e Proxy),
            ">, received none"
          ]
    Left someEx -> do
      case fromException @e someEx of
        -- 2.2: Received exception e: return logs/notes
        Just e -> (,e) <$> readRef ls
        -- 2.3: Received some other exception: print logs and die
        Nothing -> do
          printLogs ls
          error
            $ mconcat
              [ "Expected exception <",
                show (typeRep @_ @e Proxy),
                ">, but received another: ",
                displayException someEx
              ]
  where
    readRef ::
      IORef (List Text) ->
      IO (List ResultText)
    readRef ls = fmap MkResultText . L.reverse <$> readIORef ls

    printLogs :: IORef (List Text) -> IO ()
    printLogs ls = do
      logs <- readIORef ls

      putStrLn "\n*** LOGS ***\n"

      for_ logs (putStrLn . unpack)
      putStrLn ""

-- | Runs shrun potentially catching an expected exception.
runCancelled ::
  Natural ->
  List String ->
  IO (List ResultText, List ShrunNote)
runCancelled secToSleep argList = do
  ls <- newIORef []
  shrunNotes <- newIORef []

  let action = do
        withArgs argList $ Env.withEnv $ \env -> do
          let funcEnv =
                MkFuncEnv
                  { coreEnv = env,
                    funcIOEnv = MkFuncIOEnv Nothing Nothing,
                    logs = ls,
                    shrunNotes
                  }

          SR.runShellT SR.shrun funcEnv

  Async.withAsync action $ \async -> do
    sleep secToSleep
    Async.cancel async

  readRefs ls shrunNotes
  where
    readRefs ::
      IORef (List Text) ->
      IORef (List ShrunNote) ->
      IO (List ResultText, List ShrunNote)
    readRefs ls ns = ((,) . fmap MkResultText <$> readIORef ls) <*> readIORef ns

debugPrefix :: (IsString s) => s
debugPrefix = "[Debug]"

commandPrefix :: (IsString s) => s
commandPrefix = "[Command]"

errorPrefix :: (IsString s) => s
errorPrefix = "[Error]"

fatalPrefix :: (IsString s) => s
fatalPrefix = "[Fatal]"

-- | Expected timer text.
timerPrefix :: (IsString s) => s
timerPrefix = "[Timer]"

warnPrefix :: (IsString s) => s
warnPrefix = "[Warn]"

killedPrefix :: (IsString s) => s
killedPrefix = "[Killed]"

runningPrefix :: (IsString s, Semigroup s) => s
runningPrefix = warnPrefix <> " Attempting to cancel: "

waitingPrefix :: (IsString s, Semigroup s) => s
waitingPrefix = warnPrefix <> " Commands not started: "

timedOut :: (IsString s, Semigroup s) => s
timedOut = warnPrefix <> " Timed out"

-- | Expected finished prefix.
finishedPrefix :: (IsString s) => s
finishedPrefix = "[Finished] "

-- | Expected command text.
withDebugPrefix :: (IsString s, Semigroup s) => s -> s -> s
withDebugPrefix cmd txt = debugPrefix <> "[" <> cmd <> "] " <> txt

-- | Expected command text.
withCommandPrefix :: (IsString s, Semigroup s) => s -> s -> s
withCommandPrefix cmd txt = commandPrefix <> "[" <> cmd <> "] " <> txt

-- | Expected success text.
withSuccessPrefix :: (IsString s, Semigroup s) => s -> s
withSuccessPrefix txt = "[Success][" <> txt <> "] "

-- | Expected error text.
withErrorPrefix :: (IsString s, Semigroup s) => s -> s
withErrorPrefix cmd = errorPrefix <> "[" <> cmd <> "] "

-- | Expected error text.
withWarnPrefix :: (IsString s, Semigroup s) => s -> s
withWarnPrefix = ("[Warn] " <>)

-- | Expected error text.
withFatalPrefix :: (IsString s, Semigroup s) => s -> s
withFatalPrefix = ("[Fatal] " <>)

-- | Expected timing text.
withTimerPrefix :: (Semigroup a, IsString a) => a -> a
withTimerPrefix s = timerPrefix <> " " <> s

withFinishedPrefix :: (Semigroup s, IsString s) => s -> s
withFinishedPrefix = (finishedPrefix <>)

withKilledPrefix :: (Semigroup s, IsString s) => s -> s
withKilledPrefix = withPrefix killedPrefix

withPrefix :: (Semigroup a, IsString a) => a -> a -> a
withPrefix pfx s = pfx <> " " <> s

withBaseArgs :: List String -> List String
withBaseArgs as =
  withNoConfig
    $ [ "-c",
        configPath
      ]
    <> as

withNoConfig :: List String -> List String
withNoConfig as =
  [ "--config",
    "off",
    -- This is not the default, but we do not want it on.
    "--legend-keys-cache",
    "off"
  ]
    <> as

configPath :: String
#if OSX
configPath = "test" `cfp` "functional" `cfp` "example_osx.toml"
#else
configPath = "examples" `cfp` "config.toml"
#endif

notifySystemArg :: String
#if OSX
notifySystemArg = "apple-script"
#else
notifySystemArg = "notify-send"
#endif

cfp :: FilePath -> FilePath -> FilePath
cfp = combineFilePaths

readLogFile :: OsPath -> IO (List ResultText)
readLogFile path = fmap MkResultText . T.lines <$> readFileUtf8ThrowM path

appendScriptsHome :: (IsString a, Semigroup a) => a -> a
appendScriptsHome p = scriptsHomeStr <> "/" <> p

scriptsHomeStr :: (IsString a) => a
scriptsHomeStr = "test/functional/scripts"

type ShrunCons m =
  ( MonadAsync m,
    MonadDBus m,
    MonadEnv m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIO m,
    MonadIORef m,
    MonadMask m,
    MonadMVar m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixSignals m,
    MonadProcess m,
    MonadSTM m,
    MonadTerminal m,
    MonadThread m,
    MonadTime m
  )

usingReaderT :: env -> ReaderT env m a -> m a
usingReaderT = flip runReaderT
