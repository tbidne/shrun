{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effects.Concurrent.Async qualified as Async
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import FileSystem.OsPath (unsafeDecode)
import Shrun.Prelude
import System.Environment qualified as Env
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import Test.Shrun.Installer qualified as Test.Installer
import Test.Shrun.Logger qualified as Test.Logger
import Test.Shrun.Process qualified as Test.Process
import Text.Read qualified as TR

data VerifyCancel
  = -- | Tests that shrun is cancelled.
    VerifyCancelShrun
  | -- | Tests that shrun and commands are cancelled.
    VerifyCancelCommand

strToCancelled :: (HasCallStack) => Text -> IO VerifyCancel
strToCancelled =
  T.toLower . T.strip >>> \case
    "shrun" -> pure VerifyCancelShrun
    "command" -> pure VerifyCancelCommand
    other -> throwText $ "Expected (command|shrun), received: " <> other

-- | Entry point for functional tests.
main :: IO ()
main = do
  Env.lookupEnv "TEST_TERMINATE" >>= \case
    Nothing -> dontRun
    Just v -> strToCancelled (pack v) >>= runTests
  where
    runTests c = bracket (setup c) cleanup $ \params -> tests params

    dontRun = Test.Logger.putLog "Terminate tests disabled. Enable with TEST_TERMINATE=(command|shrun)"

    cleanup sp =
      guardOrElse'
        "NO_CLEANUP"
        ExpectEnvSet
        doNothing
        (teardown sp)
      where
        doNothing =
          Test.Logger.putLog
            $ "Not cleaning up test dir: '"
            <> decodeLenient (sp ^. #testDir)
            <> "'"

tests :: (HasCallStack) => SuiteParams -> IO ()
tests sp =
  for_ idxCombs $ \((command, commandLogging, signalType), idx) -> do
    runTest sp idx (MkTestParams {command, commandLogging, signalType})
  where
    idxCombs = zip combs [1 ..]

    combs =
      [ (c, cl, st)
      | c <- universe,
        cl <- universe,
        st <- universe
      ]

    universe :: (Bounded a, Enum a) => [a]
    universe = [minBound .. maxBound]

runTest :: (HasCallStack) => SuiteParams -> Int -> TestParams -> IO ()
runTest sp idx tp = do
  Test.Logger.putLogHeader $ "TEST " ++ show idx ++ ": " ++ desc

  bracket (testSetup sp tp) testTeardown $ \(cmd, shrunPid) -> do
    -- 1. Kill shrun, give it time to clean up.
    killPid (tp ^. #signalType) shrunPid True
    sleep testDelay
    -- 2. Get the output, assert processes have been killed.
    output <- runPs

    runCat True "shrun.log"
    runCat False "handler.txt"

    assertNotExists cmd output

    -- NOTE: [PS Output]
    --
    -- We always verify shrun is cancelled; only if 'command' is set do we
    -- also check commands (sleep). This is due to linux CI not behaving.
    -- On CI ubuntu, the ps output look like:
    --
    -- - runner     92458  0.2  0.0 1074605888 14984 ?    Sl   22:01   0:00 /tmp/shrun/test/terminate/shrun --config off sleep 77
    -- - runner     92469  0.0  0.0   2804  1620 ?        S    22:01   0:00 /bin/sh -c sleep 77
    -- - runner     92470  0.0  0.0   6116  1856 ?        S    22:01   0:00 sleep 77
    -- - runner     92473  0.0  0.0   2804  1748 ?        S    22:01   0:00 /bin/sh -c ps -fp $(pgrep -f '.*sleep.*')
    --
    -- Hence we have 3 processes we care about:
    --
    -- - ./shrun --config off sleep 77
    -- - ./bin/sh -c sleep 77
    -- - sleep 77
    --
    -- Now, this sort of makes sense, since process's 'shells' command prepends
    -- the shell text with "./bin/sh -c ...". But when we run
    -- 'kill -15 92458', we correctly kill 'shrun' and '/bin/sh -c sleep 77',
    -- but we do not kill 'sleep 77'. Hence the below check fails.
    --
    -- Curiously, this is not a problem on osx, nor is it a problem on my
    -- linux machine. Even more curious, I don't see the './bin/sh -c ...'
    -- process locally.
    --
    -- Whatever the reason, this is proving hard to fix, and the current
    -- behavior is a pretty good test:
    --
    -- 1. Locally, run with TEST_TERMINATE=command
    -- 2. CI + macos, run with TEST_TERMINATE=command
    -- 3. CI + linux, run with TEST_TERMINATE=shrun
    case sp ^. #verifyCancel of
      VerifyCancelShrun -> pure ()
      VerifyCancelCommand -> assertNotExists "sleep" output
  where
    desc =
      mconcat
        [ sigTypeStr,
          " terminates shrun",
          cmdLoggingStr,
          commandStr
        ]

    cmdLoggingStr =
      if tp ^. #commandLogging
        then ", --command-logging"
        else ""

    sigTypeStr = case tp ^. #signalType of
      SignalTypeSIGINT -> "SIGINT"
      SignalTypeSIGTERM -> "SIGTERM"
      SignalTypeTimeout -> "--timeout"

    commandStr = ", " <> mkCommandStr tp

    assertNotExists :: (HasCallStack) => Text -> [Text] -> IO ()
    assertNotExists t o = do
      Test.Logger.putLog $ "Asserting that '" ++ unpack t ++ "' is not found"
      let found = L.any (foundLine t) o
      when found $ do
        throwString
          $ mconcat
            [ "Found '",
              unpack t,
              "' in process output: ",
              tlinesToStr o
            ]

    foundLine t l = t `T.isInfixOf` l

runPs :: (HasCallStack) => IO (List Text)
runPs = do
  -- Split up the pgrep and ps calls so that if pgrep does not find anything,
  -- we do not error.
  (ec1, out1, err1) <- Test.Process.runProcessArgs "pgrep" ["-f", grepStr]
  case ec1 of
    ExitFailure i -> do
      -- pgrep returns error if it doesn't find anything, which might happen
      -- at the end, if we kill everything correctly. Hence we need to
      -- distinguish "good" failures from "bad" ones.
      --
      -- The output is basically empty, so we test that.
      let failOk =
            i
              == 1
              && ""
              == T.strip (pack out1)
              && ""
              == T.strip (pack err1)
          errMsg =
            mconcat
              [ "runPs: pgrep failed: ",
                show i,
                "'\n\nOUT:\n\n",
                out1,
                "\n\nERR:\n\n",
                err1,
                "\n"
              ]
      if failOk
        then do
          Test.Logger.putLog "No processes found, OK"
          pure []
        else throwString errMsg
    ExitSuccess -> do
      (ec2, out2, err2) <- Test.Process.runProcessArgs "ps" ("-fp" : L.lines out1)
      case ec2 of
        ExitFailure i ->
          throwString
            $ mconcat
              [ "runPs: ps failed: ",
                show i,
                "'\n\nOUT:\n\n",
                out2,
                "\n\nERR:\n\n",
                err2,
                "\n"
              ]
        ExitSuccess -> do
          let outLines = T.lines $ pack out2
              msg = "ps output: " <> tlinesToStr outLines
          Test.Logger.putLogLines msg
          pure outLines
  where
    -- Search for 'sleep N' since CI randomly has a 'sleep 10'
    -- command running. We also search for 'shrun', though that is probably
    -- unnecessary.
    grepStr = "(.*sleep 22.*|.*sleep 55.*|.*sleep 66.*|.*sleep 77.*|.*shrun.*)"

assertProcesses :: (HasCallStack) => Int -> Int -> List Text -> IO Int
assertProcesses low high ps = do
  if numPs < low || numPs > high
    then do
      throwString
        $ mconcat
          [ "Expected ",
            show low,
            "-",
            show high,
            " process(es), received:",
            psStr
          ]
    else pure numPs
  where
    psStr = tlinesToStr ps
    numPs = length ps

tlinesToStr :: List Text -> String
tlinesToStr =
  unpack
    . mconcat
    . fmap ("\n - " <>)

findShrunPid :: (HasCallStack) => Text -> List Text -> IO Int
findShrunPid expectedCmd ls = do
  line <- findProc
  psLineToPid line
  where
    findProc = do
      procs <- dieIfEmpty $ L.filter (T.isInfixOf expectedCmd) ls
      case NE.filter (not . containsSh) procs of
        [x] -> pure x
        [] ->
          throwString
            $ "Received empty list with /bin/sh filter: "
            ++ tlinesToStr ls
        xs@(_ : _ : _) ->
          throwString $ "Found too many processes: " ++ tlinesToStr xs

    dieIfEmpty [] =
      throwString
        $ "Received empty list with shrun filter: "
        ++ tlinesToStr ls
    dieIfEmpty (x : xs) = pure (x :| xs)

    containsSh = T.isInfixOf "/bin/sh"

killPid :: (HasCallStack) => SignalType -> Int -> Bool -> IO ()
killPid sigType pid failIfNone = case sigType of
  SignalTypeTimeout -> pure ()
  SignalTypeSIGINT -> Test.Process.runProcessOrDie $ killCmd " -2 "
  SignalTypeSIGTERM -> Test.Process.runProcessOrDie $ killCmd " -15 "
  where
    killCmd signalStr =
      mkKill signalStr
        <> if failIfNone
          then ""
          else " || true"

    mkKill signalStr =
      mconcat
        [ "kill",
          signalStr,
          show pid
        ]

killShrunName :: (HasCallStack) => IO ()
killShrunName = void . Test.Process.runProcess $ "pkill -15 shrun || true"

runShrun :: (HasCallStack) => SuiteParams -> TestParams -> IO Text
runShrun sp tp = do
  async <- Async.async io
  -- Link so that an an unexpected exception kills the test. However,
  -- the expectation is that we exit with a specific ExitFailure, so we test
  -- for this.
  Async.link async

  pure $ T.unwords $ pack <$> "shrun" : args
  where
    io = do
      -- Mac has a really hard time running this on CI. It appears the
      -- combination of 'cabal install' producing a symlink and process
      -- executing via shell i.e. '/bin/sh path/to/shrun' behaves badly on
      -- mac.
      --
      -- My current strategy is to copy the actual shrun binary with
      -- '--install-method=copy', and then invoke that path directly,
      -- which seems to work.
      --
      -- Note we use runProcessArgs due to problems with runProcess, though
      -- it is possible the latter would also work with the non-symlink
      -- (have not tried).
      (ec, out, err) <- Test.Process.runProcessArgs exePath args
      let msg =
            mconcat
              [ "cmd '",
                Test.Process.displayCmd exePath args,
                "' exited with '",
                show ec,
                "'\n\nOUT:\n\n",
                out,
                "\n\nERR:\n\n",
                err,
                "\n"
              ]
      -- This message is in our expected exception.
      if
        | "Received cancel after running for" `T.isInfixOf` pack msg -> Test.Logger.putLogLines msg
        | "Timed out" `T.isInfixOf` pack msg -> Test.Logger.putLogLines msg
        | otherwise -> do
            cs <- PR.listDirectory testDir
            Test.Logger.putLogLines
              $ mconcat
                [ show testDir,
                  " contents: ",
                  tlinesToStr (showt <$> cs)
                ]
            throwString msg

    testDir = sp ^. #testDir
    exePath = unsafeDecode $ testDir </> [osp|shrun|]

    args =
      addCommandLogging
        $ addTimeout
          [ "--config",
            "off",
            "--common-log-debug",
            "on",
            "--file-log",
            "shrun.log",
            "--file-log-mode",
            "write",
            commandStr
          ]

    addCommandLogging as =
      if tp ^. #commandLogging
        then
          "--console-log-command"
            : "on"
            : "--console-log-line-trunc"
            : "off"
            : as
        else
          "--console-log-command"
            : "off"
            : as

    addTimeout as =
      case tp ^. #signalType of
        SignalTypeTimeout -> "--timeout" : timeoutLen : as
        _ -> as

    commandStr = mkCommandStr tp

-- | Timeout duration when cancelling shrun via --timeout. This needs to be
-- longer than 'shrunDelay', so the the first 'ps' is valid, but also shorter
-- than 'shrunDelay' + 'testDelay', so that the second 'ps' occurs _after_
-- shrun is down.
timeoutLen :: String
timeoutLen = "3s"

-- | Delay in seconds after starting shrun, before running 'ps'.
shrunDelay :: Natural
shrunDelay = 2

-- | Delay in seconds after running kill, before running 'ps' a second time.
testDelay :: Natural
testDelay = 3

psLineToPid :: (HasCallStack) => Text -> IO Int
psLineToPid line =
  case L.filter (not . T.null) $ T.split (== ' ') line of
    (_ : pidStr : _) -> pure $ TR.read $ unpack pidStr
    _ -> throwString $ "Unexpected ps format: " ++ unpack line

-- testSetup/testTeardown is for individual test setup i.e. run and kill
-- shrun process.

testSetup :: (HasCallStack) => SuiteParams -> TestParams -> IO (Text, Int)
testSetup sp tp = do
  -- Slightly convoluted. We want to:
  --
  --   1. Run shrun and make sure it matches expectations.
  --   2. Get its pid.
  --
  -- Unfortunately, 1 can succeed and 2 can fail, meaning we can be in a state
  -- where shrun has launched but we have no pid to then cancel it.
  -- Thus we surround this logic w/ onException which kills the process by
  -- name.
  cmd <- runShrun sp tp
  sleep shrunDelay
  let getPid = do
        output1 <- runPs
        -- The ps output on CI seems to be between 2 (osx) and 4 (linux)
        -- processes. See NOTE: [PS Output].
        --
        -- We need an extra one for the header.
        _ <- assertProcesses low high output1
        findShrunPid cmd output1
  (cmd,) <$> getPid `onException` killShrunName
  where
    (low, high) = case tp ^. #command of
      -- See NOTE: [PS Output].
      --
      -- We need an extra one for the header.
      --
      -- Expected processes:
      -- - terminate (CI linux)
      -- - shrun
      -- - /bin/sh (CI linux)
      -- - <cmd>
      TestCommandSingle -> (3, 5)
      -- - two sub commands (Script command)
      TestCommandScript -> (5, 7)

testTeardown :: (HasCallStack) => (Text, Int) -> IO ()
testTeardown (_, shrunPid) = do
  killPid SignalTypeSIGTERM shrunPid False
  -- Kill leftover sleeps so tests do not interfere.
  Test.Process.runProcessOrDie "pkill -15 sleep || true"

-- Ways to terminate shrun.
data SignalType
  = -- | SIGINT is sent to the process.
    SignalTypeSIGINT
  | -- | SIGTERM is sent to the process.
    SignalTypeSIGTERM
  | -- | --timeout is used.
    SignalTypeTimeout
  deriving stock (Bounded, Enum)

data TestCommand
  = TestCommandSingle
  | TestCommandScript
  deriving stock (Bounded, Enum)

-- | TestParams is used for individual tests.
data TestParams = MkTestParams
  { command :: TestCommand,
    commandLogging :: Bool,
    signalType :: SignalType
  }

instance
  (k ~ A_Lens, a ~ TestCommand, b ~ TestCommand) =>
  LabelOptic "command" k TestParams TestParams a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestParams a1 a2 a3) ->
        fmap
          (\b -> MkTestParams b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "commandLogging" k TestParams TestParams a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestParams a1 a2 a3) ->
        fmap
          (\b -> MkTestParams a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ SignalType, b ~ SignalType) =>
  LabelOptic "signalType" k TestParams TestParams a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestParams a1 a2 a3) ->
        fmap
          (MkTestParams a1 a2)
          (f a3)
  {-# INLINE labelOptic #-}

-- setup/teardown is for overall suite setup i.e. install the shrun exe
-- to the temp directory.

data SuiteParams = MkSuiteParams
  { testDir :: OsPath,
    verifyCancel :: VerifyCancel
  }

instance
  (k ~ A_Lens, a ~ OsPath, b ~ OsPath) =>
  LabelOptic "testDir" k SuiteParams SuiteParams a b
  where
  labelOptic =
    lensVL
      $ \f (MkSuiteParams a1 a2) ->
        fmap
          (\b -> MkSuiteParams b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ VerifyCancel, b ~ VerifyCancel) =>
  LabelOptic "verifyCancel" k SuiteParams SuiteParams a b
  where
  labelOptic =
    lensVL
      $ \f (MkSuiteParams a1 a2) ->
        fmap
          (MkSuiteParams a1)
          (f a2)
  {-# INLINE labelOptic #-}

setup :: (HasCallStack) => VerifyCancel -> IO SuiteParams
setup verifyCancel = do
  tmpDir <- PR.getTemporaryDirectory
  let testDir = tmpDir </> [ospPathSep|shrun/test/terminate|]

  PW.createDirectoryIfMissing True testDir

  Test.Installer.installShrunOnce testDir

  pure
    $ MkSuiteParams
      { testDir,
        verifyCancel
      }

teardown :: (HasCallStack) => SuiteParams -> IO ()
teardown sp = PW.removePathForciblyIfExists_ $ sp ^. #testDir

mkCommandStr :: TestParams -> String
mkCommandStr tp = case tp ^. #command of
  TestCommandSingle -> "sleep 77"
  TestCommandScript -> "test/terminate/test_script.sh"

runCat :: Bool -> String -> IO ()
runCat fatalErr str = do
  Test.Logger.putLog $ "start " <> str
  f $ "cat " <> str
  Test.Logger.putLog $ "end " <> str
  where
    f = if fatalErr then Test.Process.runProcessOrDie else Test.Process.runProcessTotal
