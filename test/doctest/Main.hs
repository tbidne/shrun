module Main (main) where

import System.Environment.Guard (ExpectEnv (..), guardOrElse')
import Test.DocTest qualified as DocTest
import Prelude

main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DocTest.mainFromCabal "shrun" [])
    (putStrLn "*** Doc Tests disabled. Enable with RUN_DOCTEST=1")
