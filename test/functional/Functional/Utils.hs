-- | Provides utilities.
module Functional.Utils
  ( -- * Logging messages
    cancelled,
    totalTime,

    -- * Logging color prefixes
    subCommandPrefix,
    infoSuccessPrefix,
    errPrefix,

    -- * IO
    runAndGetLogs,
  )
where

import Functional.FuncEnv (FuncEnv (..))
import Functional.Prelude
import Shrun qualified as SR
import Shrun.Configuration.Env qualified as Env
import Shrun.Configuration.Env.Types (Logging (..))
import System.Environment qualified as SysEnv

-- | Expected timeout 'Text'.
cancelled :: Text
cancelled = "Timed out, cancelling remaining commands:"

-- | Expected total \"Time elapsed\"" 'Text'.
totalTime :: Text
totalTime = "[Finished] "

-- | Expected success 'Text'.
subCommandPrefix :: Text -> Text -> Text
subCommandPrefix cmd txt = "[Command][" <> cmd <> "] " <> txt

-- | Expected success 'Text'.
infoSuccessPrefix :: Text -> Text
infoSuccessPrefix txt = "[Success][" <> txt <> "] "

-- | Expected error 'Text'.
errPrefix :: Text -> Text
errPrefix txt = "[Error][" <> txt <> "] "

runAndGetLogs :: List String -> IO (IORef (List Text))
runAndGetLogs argList = do
  SysEnv.withArgs argList $ Env.withEnv $ \env -> do
    ls <- newIORef []
    consoleQueue <- newTBQueueM 1_000
    let funcEnv =
          MkFuncEnv
            { timeout = env ^. #timeout,
              -- doing this by hand since we need a different consoleLogging
              logging =
                MkLogging
                  { cmdDisplay = env ^. (#logging % #cmdDisplay),
                    cmdNameTrunc = env ^. (#logging % #cmdNameTrunc),
                    cmdLogging = env ^. (#logging % #cmdLogging),
                    consoleLogging = consoleQueue,
                    fileLogging = env ^. (#logging % #fileLogging)
                  },
              completedCmds = env ^. #completedCmds,
              commands = env ^. #commands,
              logs = ls
            }
    SR.runShellT SR.shrun funcEnv
    pure $ funcEnv ^. #logs
