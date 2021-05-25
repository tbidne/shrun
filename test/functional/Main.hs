{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Constants qualified
import Data.Functor (($>))
import Success qualified
import SuccessWithStdout qualified
import System.IO qualified as IO
import System.IO.Silently qualified as Shh
import System.Process qualified as P
import Test.Hspec (Spec)
import Test.Hspec qualified as Hspec
import Test.Tasty qualified as T
import Test.Tasty.Hspec qualified as TH
import Timeout qualified

main :: IO ()
main = tastySpec >>= T.defaultMain
  where
    tastySpec = T.testGroup "Functional Tests" <$> TH.testSpecs spec

spec :: Spec
spec = Hspec.afterAll_ tearDown $
  Hspec.beforeAll_ setup $
    Hspec.describe "Run scenarios" $ do
      Success.spec
      SuccessWithStdout.spec
      Timeout.spec

setup :: IO ()
setup =
  let proc = (P.shell "./setup_legend.sh") {P.cwd = Just Constants.workingDirectory}
   in Shh.hSilence [IO.stderr] (P.readCreateProcess proc "" $> ())

tearDown :: IO ()
tearDown =
  let proc = (P.shell "./teardown_legend.sh") {P.cwd = Just Constants.workingDirectory}
   in P.readCreateProcess proc "" Data.Functor.$> ()