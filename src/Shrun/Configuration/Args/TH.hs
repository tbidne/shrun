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

-- | Reads examples/default.toml at compile time.
getDefaultConfigTH :: Code Q (List Text)
getDefaultConfigTH = bindCode (runIO getDefault) liftTyped
  where
    getDefault = T.lines <$> readFileUtf8Lenient [osp|examples/default.toml|]
