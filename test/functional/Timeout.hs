{-# LANGUAGE ImportQualifiedPost #-}

module Timeout (spec) where

import Constants qualified
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun qualified
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Hspec (Spec, shouldSatisfy)
import Test.Hspec qualified as Hspec

spec :: Spec
spec = Hspec.it "Should time out" $ do
  result <- Shh.capture_ $ SysEnv.withArgs args ShellRun.runShell
  T.lines (T.pack result) `shouldSatisfy` allFound . foldMap sToVerifier
  where
    args = [timeout] <> commands
    commands = ["sleep 10"]
    timeout = "--timeout=5"

allFound :: Verifier -> Bool
allFound (Verifier True True) = True
allFound _ = False

sToVerifier :: Text -> Verifier
sToVerifier s
  -- verify expected timeout
  | T.isInfixOf Constants.cancelled s = mempty {foundCancelled = True}
  -- verify final counter
  | T.isInfixOf Constants.totalTime s = mempty {foundTotalTime = True}
  | otherwise = mempty

data Verifier = Verifier
  { foundCancelled :: Bool,
    foundTotalTime :: Bool
  }
  deriving (Show)

instance Semigroup Verifier where
  (Verifier a b) <> (Verifier a' b') =
    Verifier
      (a || a')
      (b || b')

instance Monoid Verifier where
  mempty = Verifier False False