-- | Property tests for ShellRun.Env.
module Props.ShellRun.Env
  ( props,
  )
where

import Data.Text qualified as T
import Data.Word (Word16)
import Hedgehog (MonadGen (..))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange
import MaxRuns (MaxRuns (..))
import ShellRun.Command (Command (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Env (CommandDisplay (..), CommandTruncation (..))
import ShellRun.Env qualified as Env
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | Entry point for ShellRun.Env property tests.
props :: TestTree
props =
  T.testGroup
    "ShellRun.Env"
    [truncationProps]

truncationProps :: TestTree
truncationProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "displayCommandTruncation result <= truncation or truncation < 3" $
    H.withTests limit $
      H.property $ do
        cmd <- H.forAll genCommand
        cmdLimit <- H.forAll genTruncate
        let truncate = MkCommandTruncation (PFin cmdLimit)
            result = Env.displayCommandTruncation truncate ShowCommand cmd
        H.annotate $ T.unpack result
        H.diff (T.length result) prop (w16ToInt cmdLimit)
  where
    prop :: Int -> Int -> Bool
    prop r l = l < 3 || r <= l

genCommand :: MonadGen m => m Command
genCommand = MkCommand Nothing <$> genCmd
  where
    range = HRange.linearFrom 1 1 50
    genCmd = HGen.text range HGen.latin1

genTruncate :: MonadGen m => m Word16
genTruncate = HGen.word16 (HRange.linearFrom 1 1 50)

w16ToInt :: Word16 -> Int
w16ToInt = fromIntegral
