{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functional test for a successful run with native logging.
module SuccessCombineStdout (spec) where

import Control.Monad.Reader qualified as MTL
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun qualified as SR
import ShellRun.Parsing.Env qualified as Env
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Hspec (Spec, shouldSatisfy)
import Test.Hspec qualified as Hspec

-- | Spec that should run commands successfully and print stdout.
spec :: Spec
spec =
  Hspec.it "Should print commands stdout" $ do
    env <- SysEnv.withArgs argList Env.runParser
    let action = MTL.runReaderT (SR.runShellT SR.runShell) env
    result <- Shh.capture_ action
    T.lines (T.pack result) `shouldSatisfy` verified . foldMap sToVerifier
  where
    argList = [nativeLog] <> commands
    commands = ["echo hi"]
    nativeLog = "--combine-sub-logs"

verified :: Verifier -> Bool
verified (Verifier True) = True
verified _ = False

sToVerifier :: Text -> Verifier
sToVerifier s
  -- verify "echo hi" stdout present
  | T.isInfixOf cmdEchoHiStdout s = mempty {foundEchoHi = True}
  | otherwise = mempty

newtype Verifier = Verifier
  { foundEchoHi :: Bool
  }
  deriving (Show)

instance Semigroup Verifier where
  (Verifier a) <> (Verifier a') =
    Verifier
      (a || a')

instance Monoid Verifier where
  mempty = Verifier False

cmdEchoHiStdout :: Text
cmdEchoHiStdout = "echo hi: hi"
