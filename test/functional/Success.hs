{-# LANGUAGE ImportQualifiedPost #-}

module Success (spec) where

import Constants qualified
import Control.Monad.Reader qualified as MTL
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun qualified as SR
import ShellRun.Parsing.Env qualified as Env
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Hspec (Spec, shouldSatisfy)
import Test.Hspec qualified as Hspec

spec :: Spec
spec =
  Hspec.it "Should run commands successfully" $ do
    env <- SysEnv.withArgs argList Env.runParser
    let action = MTL.runReaderT (SR.runShellT SR.runShell) env
    result <- Shh.capture_ action
    T.lines (T.pack result) `shouldSatisfy` verified . foldMap sToVerifier
  where
    argList = [legendPath, timeout] <> commands
    legendPath = "--legend=" <> Constants.workingDirectory <> "/output/legend.txt"
    commands = ["bad", "both", "echo hi"]
    timeout = "--timeout=5"

verified :: Verifier -> Bool
verified (Verifier True True True True True True False) = True
verified _ = False

sToVerifier :: Text -> Verifier
sToVerifier s
  -- verify expected commands
  | T.isInfixOf cmdEchoHi s = mempty {foundHi = True}
  | T.isInfixOf cmdEcho1 s = mempty {foundOne = True}
  | T.isInfixOf cmdEchoLong s = mempty {foundLong = True}
  | T.isInfixOf cmdBad s = mempty {foundBad = True}
  -- verify this occurs at least once
  | T.isInfixOf Constants.timeCmd s = mempty {foundTimeCmd = True}
  -- verify final counter
  | T.isInfixOf Constants.totalTime s = mempty {foundTotalTime = True}
  -- verify "echo hi" stdout _not_ present
  | T.isInfixOf cmdEchoHiStdout s = mempty {foundEchoHi = True}
  | otherwise = mempty

data Verifier = Verifier
  { foundHi :: Bool,
    foundOne :: Bool,
    foundLong :: Bool,
    foundBad :: Bool,
    foundTimeCmd :: Bool,
    foundTotalTime :: Bool,
    foundEchoHi :: Bool
  }
  deriving (Show)

instance Semigroup Verifier where
  (Verifier a b c d e f g) <> (Verifier a' b' c' d' e' f' g') =
    Verifier
      (a || a')
      (b || b')
      (c || c')
      (d || d')
      (e || e')
      (f || f')
      (g || g')

instance Monoid Verifier where
  mempty = Verifier False False False False False False False

cmdBad :: Text
cmdBad = Constants.errPrefix <> "Error running `some nonsense`"

cmdEchoHi :: Text
cmdEchoHi = Constants.infoSuccessPrefix <> "Successfully ran `echo hi`"

cmdEcho1 :: Text
cmdEcho1 = Constants.infoSuccessPrefix <> "Successfully ran `sleep 1 && echo 1`"

cmdEchoLong :: Text
cmdEchoLong = Constants.infoSuccessPrefix <> "Successfully ran `sleep 2 && echo long`"

cmdEchoHiStdout :: Text
cmdEchoHiStdout = "echo hi: hi"
