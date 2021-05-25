{-# LANGUAGE ImportQualifiedPost #-}

module SuccessWithStdout (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import ShellRun qualified
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Hspec (Spec, shouldSatisfy)
import Test.Hspec qualified as Hspec

spec :: Spec
spec =
  Hspec.it "Should print commands stdout" $ do
    result <- Shh.capture_ $ SysEnv.withArgs args ShellRun.runShell
    T.lines (T.pack result) `shouldSatisfy` verified . foldMap sToVerifier
  where
    args = [nativeLog] <> commands
    commands = ["echo hi"]
    nativeLog = "--nativeLog"

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