{-# LANGUAGE QuasiQuotes #-}

-- | Provides TH for CLI args.
module Shrun.Configuration.Args.TH
  ( getDefaultConfigTH,
  )
where

import Data.Text qualified as T
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped), bindCode, runIO)
import Shrun.Prelude

-- TODO: Once we have multiline strings, it may be feasible to remove the TH
-- here.

-- | Reads examples/default.toml at compile time.
getDefaultConfigTH :: Code Q (List Text)
getDefaultConfigTH = bindCode (runIO getDefault) liftTyped
  where
    getDefault = T.lines <$> readFileUtf8Lenient [osp|examples/default.toml|]
