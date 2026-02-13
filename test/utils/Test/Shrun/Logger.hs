{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitPrelude #-}

module Test.Shrun.Logger
  ( putLog,
    putLogHeader,
    putLogLines,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Effects.FileSystem.FileWriter (ByteString)
import Effects.FileSystem.FileWriter qualified as FW
import FileSystem.OsPath (OsPath, osp, (</>))
import FileSystem.UTF8 qualified as UTF8

putLog :: OsPath -> String -> IO ()
putLog p s = writeStr p $ "\n*** " ++ s ++ " ***"

putLogLines :: OsPath -> String -> IO ()
putLogLines p s =
  writeStr p $
    L.unlines
      [ hs,
        s,
        hs
      ]
  where
    hs = L.replicate 80 '-'

putLogHeader :: OsPath -> String -> IO ()
putLogHeader p s =
  writeStr p $
    L.unlines
      [ hs,
        s,
        hs
      ]
  where
    hs = L.replicate num '*'

    num = max 80 (L.length s)

writeStr :: OsPath -> String -> IO ()
writeStr p = FW.appendBinaryFile p' . strToBs
  where
    p' = mkLogPath p

strToBs :: String -> ByteString
strToBs = UTF8.encodeUtf8 . T.pack

mkLogPath :: OsPath -> OsPath
mkLogPath testDir = testDir </> [osp|log|]
