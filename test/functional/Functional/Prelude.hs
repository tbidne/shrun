{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO:
--
--   1. Once our cabal / cabal-fmt recognizes TypeAbstractions, add it to the
--      cabal file instead i.e.
--
--          if impl(ghc >=9.8.1)
--            default-extensions: TypeAbstractions
--
--   2. Once we only support GHC 9.8+, add it unconditionally to
--      default-extensions.
--
--   3. It would be nice if we could test that we do not receive any "extra"
--      output e.g. "ExitFailure 1". To do this, though, we'd have to test
--      with the exception logic, since the exception stuff happens _outside_
--      of these tests i.e. exceptions are caught and the unwanted output will
--      occur from the handler (set in Main.hs).
--
--  4. Consider testing --help (would require some refactoring like 3 above).

#if __GLASGOW_HASKELL__ >= 908
{-# LANGUAGE TypeAbstractions #-}
#endif

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
import Shrun.Configuration.Env.Types
  ( Env,
    HasAnyError (getAnyError),
    HasCommandLogging (getCommandLogging),
    HasCommands (getCommandDepGraph, getCommandStatus),
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit (getInit),
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getTimeout),
  )
import Shrun.Logging.MonadRegionLogger
  ( MonadRegionLogger
      ( Region,
        displayRegions,
        logGlobal,
        logRegion,
        regionList,
        withRegion
      ),
  )
import Shrun.Notify.MonadNotify (MonadNotify (notify), ShrunNote)
import Shrun.Prelude as X
import Shrun.ShellT (ShellT)
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

-- NOTE: FuncEnv is essentially the real Env w/ an IORef for logs and a
-- simplified logging

data FuncEnv = MkFuncEnv
  { coreEnv :: Env (),
    logs :: IORef (List Text),
    shrunNotes :: IORef (List ShrunNote)
  }

instance
  ( k ~ A_Lens,
    a ~ Env (),
    b ~ Env ()
  ) =>
  LabelOptic "coreEnv" k FuncEnv FuncEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkFuncEnv a1 a2 a3) ->
        fmap
          (\b -> MkFuncEnv b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ IORef (List Text),
    b ~ IORef (List Text)
  ) =>
  LabelOptic "logs" k FuncEnv FuncEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkFuncEnv a1 a2 a3) ->
        fmap
          (\b -> MkFuncEnv a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ IORef (List ShrunNote),
    b ~ IORef (List ShrunNote)
  ) =>
  LabelOptic "shrunNotes" k FuncEnv FuncEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkFuncEnv a1 a2 a3) ->
        fmap
          (\b -> MkFuncEnv a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

instance HasTimeout FuncEnv where
  getTimeout = getTimeout . view #coreEnv

instance HasInit FuncEnv where
  getInit = getInit . view #coreEnv

instance HasCommands FuncEnv where
  getCommandDepGraph = getCommandDepGraph . view #coreEnv
  getCommandStatus = getCommandStatus . view #coreEnv

instance HasAnyError FuncEnv where
  getAnyError = getAnyError . view #coreEnv

instance HasCommandLogging FuncEnv where
  getCommandLogging = getCommandLogging . view #coreEnv

instance HasCommonLogging FuncEnv where
  getCommonLogging = getCommonLogging . view #coreEnv

instance HasConsoleLogging FuncEnv () where
  getConsoleLogging = getConsoleLogging . view #coreEnv

instance HasFileLogging FuncEnv where
  getFileLogging = getFileLogging . view #coreEnv

instance HasNotifyConfig FuncEnv where
  getNotifyConfig = getNotifyConfig . view #coreEnv

instance MonadRegionLogger (ShellT FuncEnv IO) where
  type Region (ShellT FuncEnv IO) = ()

  logGlobal txt = do
    ls <- asks $ view #logs
    liftIO $ modifyIORef' ls (txt :)

  logRegion _ _ = logGlobal

  withRegion _layout regionToShell = regionToShell ()

  displayRegions = id

  regionList = atomically $ newTMVar []

instance MonadNotify (ShellT FuncEnv IO) where
  notify note = do
    notesRef <- asks (view #shrunNotes)
    modifyIORef' notesRef (note :)
    pure Nothing

-- | Runs the args and retrieves the logs.
run :: List String -> IO (List ResultText)
run = fmap fst . runMaybeException ExNothing

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
  MaybeException ->
  List String ->
  IO (List ResultText, List ShrunNote)
runMaybeException mException argList = do
  ls <- newIORef []
  shrunNotes <- newIORef []

  let action = do
        withArgs argList $ Env.withEnv $ \env -> do
          let funcEnv =
                MkFuncEnv
                  { coreEnv = env,
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
      IO (List ResultText, List ShrunNote)
    readRefs ls ns = ((,) . fmap MkResultText . L.reverse <$> readIORef ls) <*> readIORef ns

    printLogsReThrow :: (Exception e) => e -> IORef (List Text) -> IO void
    printLogsReThrow ex ls = do
      printLogs ls

      -- rethrow
      throwM ex

    printLogs :: IORef (List Text) -> IO ()
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
  [ "-c",
    configPath
  ]
    <> as

withNoConfig :: List String -> List String
withNoConfig as =
  [ "--config",
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
