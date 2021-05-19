{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.Functor (($>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun qualified
import System.IO qualified as IO
import System.IO.Silently qualified as Shh
import System.Process qualified as P
import Test.Hspec (Spec, shouldSatisfy)
import Test.Hspec qualified as Hspec
import Test.Tasty qualified as T
import Test.Tasty.Hspec qualified as TH
import System.Environment qualified as SysEnv

main :: IO ()
main = tastySpec >>= T.defaultMain
  where
    tastySpec = T.testGroup "Functional Tests" <$> TH.testSpecs spec

spec :: Spec
spec = Hspec.afterAll_ tearDown $
  Hspec.beforeAll_ setup $
    Hspec.describe "" $ do
      Hspec.it "Should run commands" $ do
            result <- Shh.capture_ $ SysEnv.withArgs args ShellRun.runShell
            T.lines (T.pack result) `shouldSatisfy` allFound . foldMap sToVerifier
  where
    args = [legendPath, timeout] <> commands
    legendPath = "--legend=" <> workingDirectory <> "/output/legend.txt"
    commands = ["bad", "both", "echo hi"]
    timeout = "--timeout=5"

workingDirectory :: IsString a => a
workingDirectory = "./test/functional/scripts"

setup :: IO ()
setup =
  let proc = (P.shell "./setup_legend.sh") {P.cwd = Just workingDirectory}
   in Shh.hSilence [IO.stderr] (P.readCreateProcess proc "" $> ())

tearDown :: IO ()
tearDown =
  let proc = (P.shell "./teardown_legend.sh") {P.cwd = Just workingDirectory}
   in P.readCreateProcess proc "" Data.Functor.$> ()

allFound :: Verifier -> Bool
allFound (Verifier True True True True True True) = True
allFound _ = False

sToVerifier :: Text -> Verifier
sToVerifier s
  -- verify expected commands
  | T.isInfixOf cmdEchoHi s = mempty {foundHi = True}
  | T.isInfixOf cmdEcho1 s = mempty {foundOne = True}
  | T.isInfixOf cmdEchoLong s = mempty {foundLong = True}
  | T.isInfixOf cmdBad s = mempty {foundBad = True}
  -- verify this occurs at least once
  | T.isInfixOf timeCmd s = mempty {foundTimeCmd = True}
  -- verify final counter
  | T.isInfixOf totalTime s = mempty {foundTotalTime = True}
  | otherwise = mempty

data Verifier = Verifier
  { foundHi :: Bool,
    foundOne :: Bool,
    foundLong :: Bool,
    foundBad :: Bool,
    foundTimeCmd :: Bool,
    foundTotalTime :: Bool
  }
  deriving (Show)

instance Semigroup Verifier where
  (Verifier a b c d e f) <> (Verifier a' b' c' d' e' f') =
    Verifier
      (a || a')
      (b || b')
      (c || c')
      (d || d')
      (e || e')
      (f || f')

instance Monoid Verifier where
  mempty = Verifier False False False False False False

cmdBad :: Text
cmdBad = errPrefix <> "Error running `some nonsense`"

cmdEchoHi :: Text
cmdEchoHi = infoSuccessPrefix <> "Successfully ran `echo hi`"

cmdEcho1 :: Text
cmdEcho1 = infoSuccessPrefix <> "Successfully ran `sleep 1 && echo 1`"

cmdEchoLong :: Text
cmdEchoLong = infoSuccessPrefix <> "Successfully ran `sleep 2 && echo long`"

timeCmd :: Text
timeCmd = infoSuccessPrefix <> "Time elapsed: "

totalTime :: Text
totalTime = infoBluePrefix <> "Total time elapsed: "

infoSuccessPrefix :: Text
infoSuccessPrefix = "\ESC[92m[Info] "

infoBluePrefix :: Text
infoBluePrefix = "\ESC[94m[Info] "

errPrefix :: Text
errPrefix = "\ESC[91m[Error] "
