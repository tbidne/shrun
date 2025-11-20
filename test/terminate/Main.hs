{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effects.Concurrent.Async qualified as Async
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import Effects.System.Process qualified as P
import FileSystem.OsPath (unsafeDecode)
import Shrun.Prelude
import System.Environment qualified as Env
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import Text.Read qualified as TR

data VerifyCancel
  = -- | Tests that shrun is cancelled.
    VerifyCancelShrun
  | -- | Tests that shrun and commands are cancelled.
    VerifyCancelCommand

strToCancelled :: Text -> IO VerifyCancel
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

    dontRun = putLog "Terminate tests disabled. Enable with TEST_TERMINATE=(command|shrun)"

    cleanup sp =
      guardOrElse'
        "NO_CLEANUP"
        ExpectEnvSet
        doNothing
        (teardown sp)
      where
        doNothing =
          putLog
            $ "Not cleaning up test dir: '"
            <> decodeLenient (sp ^. #testDir)
            <> "'"

tests :: SuiteParams -> IO ()
tests sp =
  for_ combs $ \((commandLogging, signalType), idx) -> do
    runTest sp idx (MkTestParams {commandLogging, signalType})
  where
    combs = zip [(cl, st) | cl <- universe, st <- universe] [1 ..]

    universe :: (Bounded a, Enum a) => [a]
    universe = [minBound .. maxBound]

runTest :: SuiteParams -> Int -> TestParams -> IO ()
runTest sp idx tp = do
  putLog $ "TEST " ++ show idx ++ ": " ++ desc

  bracket (testSetup sp tp) testTeardown $ \(cmd, shrunPid) -> do
    -- 1. Kill shrun, give it time to clean up.
    killPid (tp ^. #signalType) shrunPid True
    sleep 2
    -- 2. Get the output, assert processes have been killed.
    output <- runPs
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
          " terminates shrun ",
          cmdLoggingStr
        ]

    cmdLoggingStr =
      if tp ^. #commandLogging
        then "with --command-logging"
        else "without --command-logging"

    sigTypeStr = case tp ^. #signalType of
      SignalTypeSIGINT -> "SIGINT"
      SignalTypeSIGTERM -> "SIGTERM"

    assertNotExists :: Text -> [Text] -> IO ()
    assertNotExists t o = do
      putLog $ "Asserting that '" ++ unpack t ++ "' is not found"
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

runPs :: IO (List Text)
runPs = do
  -- Split up the pgrep and ps calls so that if pgrep does not find anything,
  -- we do not error. Search for 'sleep 77' since CI randomly has a 'sleep 10'
  -- command running. We also search for 'shrun', though that is probably
  -- unnecessary.
  (ec1, out1, err1) <- runProcessArgs "pgrep" ["-f", "(.*sleep 77.*|.*shrun.*)"]
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
          putLog "No processes found, OK"
          pure []
        else throwString errMsg
    ExitSuccess -> do
      (ec2, out2, err2) <- runProcessArgs "ps" ("-fp" : L.lines out1)
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
          putLogLines msg
          pure outLines

assertProcesses :: Int -> Int -> List Text -> IO Int
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

findShrunPid :: Text -> List Text -> IO Int
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

killPid :: SignalType -> Int -> Bool -> IO ()
killPid sigType pid failIfNone = runProcessOrDie killCmd
  where
    killCmd =
      mkKill
        <> if failIfNone
          then ""
          else " || true"

    mkKill =
      mconcat
        [ "kill",
          signalStr,
          show pid
        ]

    signalStr = case sigType of
      SignalTypeSIGINT -> " -2 "
      SignalTypeSIGTERM -> " -15 "

killShrunName :: IO ()
killShrunName = void . runProcess $ "pkill -15 shrun || true"

runShrun :: SuiteParams -> TestParams -> IO Text
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
      (ec, out, err) <- runProcessArgs exePath args
      let msg =
            mconcat
              [ "cmd '",
                displayCmd exePath args,
                "' exited with '",
                show ec,
                "'\n\nOUT:\n\n",
                out,
                "\n\nERR:\n\n",
                err,
                "\n"
              ]
      -- This message is in our expected exception.
      if "Received cancel after running for" `T.isInfixOf` pack msg
        then putLogLines msg
        else do
          cs <- PR.listDirectory testDir
          putLogLines
            $ mconcat
              [ show testDir,
                " contents: ",
                tlinesToStr (showt <$> cs)
              ]
          throwString msg

    testDir = sp ^. #testDir
    exePath = unsafeDecode $ testDir </> [osp|shrun|]

    args =
      if tp ^. #commandLogging
        then ["--config", "off", "--common-log-debug", "on", "--console-log-command", "on", "sleep 77"]
        else ["--config", "off", "--common-log-debug", "on", "sleep 77"]

runProcess :: String -> IO (ExitCode, String, String)
runProcess txt = do
  putLog $ "Running '" ++ txt ++ "'"
  tryMySync (P.readCreateProcessWithExitCode (P.shell txt) "runProcess") >>= \case
    Right r -> pure r
    Left err -> pure (ExitFailure 1, "Exception running command: " ++ txt, displayException err)

runProcessArgs :: String -> [String] -> IO (ExitCode, String, String)
runProcessArgs cmd args = do
  putLog $ "Running '" ++ displayCmd cmd args ++ "'"
  tryMySync (P.readProcessWithExitCode cmd args "runProcessArgs") >>= \case
    Right r -> pure r
    Left err -> pure (ExitFailure 1, "Exception running command: " ++ cmd, displayException err)

runProcessOrDie :: String -> IO ()
runProcessOrDie txt = do
  (ec, out, err) <- runProcess txt
  case ec of
    ExitSuccess ->
      putStrLn
        $ mconcat
          [ "Process '",
            txt,
            "' succeeded. Stdout: '",
            out,
            "', stderr: '",
            err,
            "'"
          ]
    ExitFailure _ -> do
      throwString
        $ mconcat
          [ "Process '",
            txt,
            "' failed. Stdout: '",
            out,
            "', stderr: '",
            err,
            "'"
          ]

psLineToPid :: Text -> IO Int
psLineToPid line =
  case L.filter (not . T.null) $ T.split (== ' ') line of
    (_ : pidStr : _) -> pure $ TR.read $ unpack pidStr
    _ -> throwString $ "Unexpected ps format: " ++ unpack line

-- testSetup/testTeardown is for individual test setup i.e. run and kill
-- shrun process.

testSetup :: SuiteParams -> TestParams -> IO (Text, Int)
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
  sleep 2
  let getPid = do
        output1 <- runPs
        -- The ps output on CI seems to be between 2 (osx) and 4 (linux)
        -- processes. See NOTE: [PS Output].
        --
        -- We need an extra one for the header.
        _ <- assertProcesses 2 5 output1
        findShrunPid cmd output1
  (cmd,) <$> getPid `onException` killShrunName

testTeardown :: (Text, Int) -> IO ()
testTeardown (_, shrunPid) = do
  killPid SignalTypeSIGTERM shrunPid False
  -- Kill leftover sleeps so tests do not interfere.
  runProcessOrDie "pkill -15 sleep || true"

installShrun :: OsPath -> IO ()
installShrun d = runProcessOrDie cmd
  where
    cmd =
      mconcat
        [ "export SHRUN_HOME=$(pwd); ",
          "cabal install exe:shrun ",
          "--installdir=",
          dStr,
          " --install-method=copy ",
          "--overwrite-policy=always"
        ]

    dStr = unsafeDecode d

data SignalType
  = SignalTypeSIGINT
  | SignalTypeSIGTERM
  deriving stock (Bounded, Enum)

-- | TestParams is used for individual tests.
data TestParams = MkTestParams
  { commandLogging :: Bool,
    signalType :: SignalType
  }

instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "commandLogging" k TestParams TestParams a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestParams a1 a2) ->
        fmap
          (\b -> MkTestParams b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ SignalType, b ~ SignalType) =>
  LabelOptic "signalType" k TestParams TestParams a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestParams a1 a2) ->
        fmap
          (MkTestParams a1)
          (f a2)
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

setup :: VerifyCancel -> IO SuiteParams
setup verifyCancel = do
  tmpDir <- PR.getTemporaryDirectory
  let testDir = tmpDir </> [ospPathSep|shrun/test/terminate|]
      exeExpectedPath = testDir </> [osp|shrun|]

  PW.createDirectoryIfMissing True testDir

  mExePath <- PR.findExecutable exeExpectedPath
  case mExePath of
    Just _ -> pure ()
    Nothing -> installShrun testDir

  pure
    $ MkSuiteParams
      { testDir,
        verifyCancel
      }

teardown :: SuiteParams -> IO ()
teardown sp = PW.removePathForciblyIfExists_ $ sp ^. #testDir

displayCmd :: String -> List String -> String
displayCmd cmd args =
  mconcat
    [ "command: '",
      cmd,
      "', args: ",
      show args
    ]

putLog :: String -> IO ()
putLog s = putStrLn $ "\n*** " ++ s ++ " ***"

putLogLines :: String -> IO ()
putLogLines s =
  putStrLn
    $ L.unlines
      [ hs,
        s,
        hs
      ]
  where
    hs = L.replicate 80 '-'
