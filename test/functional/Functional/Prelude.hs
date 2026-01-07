{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

{- HLINT ignore "Functor law" -}

-- TODO:
--
--   1. It would be nice if we could test that we do not receive any "extra"
--      output e.g. "ExitFailure 1". To do this, though, we'd have to test
--      with the exception logic, since the exception stuff happens _outside_
--      of these tests i.e. exceptions are caught and the unwanted output will
--      occur from the handler (set in Main.hs).

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
    runExitConfigLogs,

    -- ** Mocked IO (Configuration)
    ConfigIOEnv (..),
    runConfigIO,

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
    withDebugNoCmdPrefix,
    withSuccessPrefix,
    withErrorPrefix,
    withWarnPrefix,
    withWarnCmdPrefix,
    withFatalPrefix,
    withTimerPrefix,
    withFinishedPrefix,
    withKilledPrefix,

    -- * Misc
    assertList,
    configPath,
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
  ( ConfigIOEnv (MkConfigIOEnv, cwdDir, logs, xdgDir),
    FuncEnv (MkFuncEnv, coreEnv, logs, shrunNotes),
    unConfigIO,
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
run = fmap (view _2) . baseRunner @SomeException Nothing Nothing

-- | Like 'run', but with 'ConfigIO' instead of normal 'IO'.
runConfigIO :: ConfigIOEnv -> List String -> IO (List ResultText)
runConfigIO env = fmap (view _2) . baseRunner @SomeException (Just env) Nothing

-- | Runs the args and retrieves the sent notifications.
runNotes :: List String -> IO (List ShrunNote)
runNotes = fmap (view _3) . baseRunner @SomeException Nothing Nothing

runExitConfigLogs :: List String -> IO (List Text)
runExitConfigLogs = fmap (view _1) . baseRunner Nothing (Just $ Proxy @ExitCode)

-- | 'runException' specialized to ExitFailure.
runExitFailure :: List String -> IO (List ResultText)
runExitFailure =
  fmap (view _2)
    . baseRunner Nothing (Just $ Proxy @ExitCode)

runAllExitFailure :: List String -> IO (Tuple2 (List ResultText) (List ShrunNote))
runAllExitFailure =
  fmap (\(_, y, z, _) -> (y, z))
    . baseRunner Nothing (Just $ Proxy @ExitCode)

-- | Like 'runException', except it expects an exception.
runException ::
  forall e.
  (Exception e) =>
  List String ->
  IO (List ResultText)
runException = fmap (view _2) . baseRunner Nothing (Just $ Proxy @e)

-- | Runs shrun potentially catching an expected exception.
baseRunner ::
  forall e.
  (Exception e) =>
  Maybe ConfigIOEnv ->
  Maybe (Proxy e) ->
  List String ->
  IO (List Text, List ResultText, List ShrunNote, Maybe e)
baseRunner mConfigIOEnv mExProxy argList = do
  (action, configLogs, ls, shrunNotes) <- mkShrunAction mConfigIOEnv argList

  case mExProxy of
    -- 1. Not expecting an exception
    Nothing -> do
      tryMySync action >>= \case
        -- 1.1: Received an exception: print logs and rethrow
        Left ex -> printLogsReThrow ex ls
        -- 1.2: No exception, return logs/notes
        Right _ -> (\(x, y, z) -> (x, y, z, Nothing)) <$> readRefs configLogs ls shrunNotes
    -- 2. Expecting exception e
    Just proxy ->
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
            Just e2 -> (\(x, y, z) -> (x, y, z, Just e2)) <$> readRefs configLogs ls shrunNotes
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
    printLogsReThrow :: (Exception e2) => e2 -> IORef (List Text) -> IO void
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
  (_, results, _, mEx) <- baseRunner Nothing (Just $ Proxy @e) argList
  case mEx of
    Nothing ->
      -- This is probably impossible
      error "Expected ex, received nothing"
    Just ex -> pure (results, ex)

-- | Runs shrun potentially catching an expected exception.
runCancelled ::
  Natural ->
  List String ->
  IO (List ResultText, List ShrunNote)
runCancelled secToSleep argList = do
  (action, configLogs, ls, shrunNotes) <- mkShrunAction Nothing argList

  Async.withAsync action $ \async -> do
    sleep secToSleep
    Async.cancel async

  (\(_, y, z) -> (y, z)) <$> readRefs configLogs ls shrunNotes

mkShrunAction ::
  Maybe ConfigIOEnv ->
  List String ->
  IO (Tuple4 (IO ()) (IORef (List Text)) (IORef (List Text)) (IORef (List ShrunNote)))
mkShrunAction mConfigIOEnv argList = do
  configLogsRef <- newIORef []
  ls <- newIORef []
  shrunNotes <- newIORef []

  configIOEnv <- case mConfigIOEnv of
    Nothing -> do
      pure
        $ MkConfigIOEnv
          { cwdDir = Nothing,
            logs = configLogsRef,
            xdgDir = Nothing
          }
    Just cfg -> pure cfg

  -- Always run the config stage via ConfigIO.
  let action = do
        usingReaderT configIOEnv
          . unConfigIO
          . withArgs argList
          . Env.withEnv
          $ \env -> do
            let funcEnv =
                  MkFuncEnv
                    { coreEnv = env,
                      logs = ls,
                      shrunNotes
                    }

            SR.runShellT SR.shrun funcEnv

  pure (action, configLogsRef, ls, shrunNotes)

readRefs ::
  IORef (List Text) ->
  IORef (List Text) ->
  IORef (List ShrunNote) ->
  IO (List Text, List ResultText, List ShrunNote)
readRefs configLogs ls ns =
  (,,)
    <$> (L.reverse <$> readIORef configLogs)
    <*> (fmap MkResultText . L.reverse <$> readIORef ls)
    <*> readIORef ns

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
runningPrefix = warnPrefix <> " Attempting to cancel:"

waitingPrefix :: (IsString s, Semigroup s) => s
waitingPrefix = warnPrefix <> " Commands not started:"

timedOut :: (IsString s, Semigroup s) => s
timedOut = warnPrefix <> " Timed out"

-- | Expected finished prefix.
finishedPrefix :: (IsString s) => s
finishedPrefix = "[Finished] "

-- | Expected command text.
withDebugPrefix :: (IsString s, Semigroup s) => s -> s -> s
withDebugPrefix cmd txt = debugPrefix <> "[" <> cmd <> "] " <> txt

withDebugNoCmdPrefix :: (IsString s, Semigroup s) => s -> s
withDebugNoCmdPrefix txt = debugPrefix <> " " <> txt

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

withWarnCmdPrefix :: (IsString s, Semigroup s) => s -> s
withWarnCmdPrefix cmd = "[Warn][" <> cmd <> "] "

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

notifySystemArg :: (IsString s) => s
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

usingReaderT :: env -> ReaderT env m a -> m a
usingReaderT = flip runReaderT

assertList :: (Eq a, Show a) => List a -> List a -> IO ()
assertList = go
  where
    go [] [] = pure ()
    go lhs@(_ : _) [] = assertFailure $ "LHS nonempty: " ++ show lhs
    go [] rhs@(_ : _) = assertFailure $ "RHS nonempty: " ++ show rhs
    go (x : xs) (y : ys) = (x @=? y) *> go xs ys
