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
import System.Environment qualified as SysEnv

-- | Expected timeout 'Text'.
cancelled :: Text
cancelled = "Timed out, cancelling remaining commands:"

-- | Expected total \"Time elapsed\"" 'Text'.
totalTime :: Text
totalTime = "Finished! Total time elapsed: "

-- | Expected success 'Text'.
subCommandPrefix :: Text -> Text -> Text
subCommandPrefix cmd txt = "[Command] [" <> cmd <> "] " <> txt

-- | Expected success 'Text'.
infoSuccessPrefix :: Text -> Text
infoSuccessPrefix txt = "[Info] [" <> txt <> "] Success. Time elapsed:"

-- | Expected error 'Text'.
errPrefix :: Text -> Text
errPrefix txt = "[Error] [" <> txt <> "] Error:"

runAndGetLogs :: List String -> IO (IORef (List Text))
runAndGetLogs argList = do
  SysEnv.withArgs argList $ Env.withEnv $ \env -> do
    ls <- newIORef []
    let funcEnv =
          MkFuncEnv
            { coreEnv = env,
              logs = ls
            }
    SR.runShellT SR.shrun funcEnv
    pure $ funcEnv ^. #logs
