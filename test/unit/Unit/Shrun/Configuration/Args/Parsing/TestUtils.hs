module Unit.Shrun.Configuration.Args.Parsing.TestUtils
  ( -- * Verification
    verifyResult,
    verifyResultT,
    verifyFailure,
    verifyFailureString,

    -- * Utils
    execParserUnit,
    updateDefArgs,
    updateDefArgsWD,
    disableDefArgs,
    updateDefCoreArgs,
    updateDefCoreArgsWD,
    disableDefCoreArgs,

    -- * Defaults
    defArgs,
    defCommand,
  )
where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Options.Applicative (ParserPrefs, ParserResult)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args qualified as Args
import Shrun.Configuration.Args.Parsing (Args, parserInfoArgs)
import Shrun.Configuration.Data.Core (CoreConfigArgs)
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Unit.Prelude

execParserUnit :: List String -> ParserResult Args
execParserUnit = OA.execParserPure prefs (parserInfoArgs [])

verifyResult :: List String -> Maybe Args -> Property
verifyResult argList =
  withTests 1
    . property
    . verifyResultT argList

verifyResultT :: [String] -> Maybe Args -> PropertyT IO ()
verifyResultT argList expected = do
  let parseResult = execParserUnit argList

  annotateShow argList

  result <- case parseResult of
    OA.Success x -> pure $ Just x
    OA.Failure f -> do
      annotate $ fst $ OA.renderFailure f "Failed parsing"
      failure
    OA.CompletionInvoked _ -> failure

  expected === result

verifyFailure :: List String -> Property
verifyFailure argList = withTests 1 $ property $ do
  let parseResult = execParserUnit argList

  case parseResult of
    OA.Success _ -> failure
    OA.Failure _ -> pure ()
    OA.CompletionInvoked _ -> failure

verifyFailureString :: List String -> Text -> Property
verifyFailureString argList expected = withTests 1 $ property $ do
  let parseResult = execParserUnit argList

  case parseResult of
    OA.Success _ -> failure
    OA.Failure f -> do
      let (errStr, _) = OA.renderFailure f ""
      annotate (unpack expected)
      annotate errStr
      assert (expected `T.isPrefixOf` pack errStr)
    OA.CompletionInvoked _ -> failure

prefs :: ParserPrefs
prefs = OA.prefs mempty

defCommand :: NESeq Text
defCommand = "command" :<|| Seq.empty

defArgs :: Maybe Args
defArgs = Just $ Args.defaultArgs defCommand

updateDefArgs ::
  forall a.
  Lens' Args (Maybe a) ->
  a ->
  Maybe Args
updateDefArgs l x = (l' ?~ x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (Maybe a)
    l' = _Just % l

updateDefArgsWD ::
  forall a.
  Lens' Args (WithDisabled a) ->
  a ->
  Maybe Args
updateDefArgsWD l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % l

disableDefArgs ::
  forall a.
  Lens' Args (Maybe (WithDisabled a)) ->
  Maybe Args
disableDefArgs l = (l' ?~ Disabled) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (Maybe (WithDisabled a))
    l' = _Just % l

updateDefCoreArgs ::
  forall a.
  Lens' CoreConfigArgs (Maybe a) ->
  a ->
  Maybe Args
updateDefCoreArgs l x = (l' ?~ x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (Maybe a)
    l' = _Just % #coreConfig % l

updateDefCoreArgsWD ::
  forall a.
  Lens' CoreConfigArgs (WithDisabled a) ->
  a ->
  Maybe Args
updateDefCoreArgsWD l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % l

disableDefCoreArgs ::
  forall a.
  Lens' CoreConfigArgs (Maybe (WithDisabled a)) ->
  Maybe Args
disableDefCoreArgs l = (l' ?~ Disabled) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (Maybe (WithDisabled a))
    l' = _Just % #coreConfig % l
