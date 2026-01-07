{-# LANGUAGE ImplicitPrelude #-}

module Test.Shrun.Logger
  ( putLog,
    putLogHeader,
    putLogLines,
  )
where

import Data.List qualified as L

putLog :: String -> IO ()
putLog s = putStrLn $ "\n*** " ++ s ++ " ***"

putLogLines :: String -> IO ()
putLogLines s =
  putStrLn $
    L.unlines
      [ hs,
        s,
        hs
      ]
  where
    hs = L.replicate 80 '-'

putLogHeader :: String -> IO ()
putLogHeader s =
  putStrLn $
    L.unlines
      [ hs,
        s,
        hs
      ]
  where
    hs = L.replicate num '*'

    num = max 80 (L.length s)
