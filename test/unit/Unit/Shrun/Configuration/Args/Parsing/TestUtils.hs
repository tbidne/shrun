module Unit.Shrun.Configuration.Args.Parsing.TestUtils
  ( -- * Verification
    verifyResult,
    verifyFailure,

    -- * Utils
    updateDefArgs,
    disableDefArgs,
    updateDefCoreArgs,
    disableDefCoreArgs,

    -- * Defaults
    defArgs,
    defCommand,
  )
where

import Data.Sequence qualified as Seq
import Options.Applicative (ParserPrefs)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args qualified as Args
import Shrun.Configuration.Args.Parsing (Args, parserInfoArgs)
import Shrun.Configuration.Data.Core (CoreConfigArgs)
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Unit.Prelude

verifyResult :: List String -> Maybe Args -> Property
verifyResult argList expected = withTests 1 $ property $ do
  let parseResult = OA.execParserPure prefs parserInfoArgs argList

  result <- case parseResult of
    OA.Success x -> pure $ Just x
    OA.Failure f -> do
      annotate $ fst $ OA.renderFailure f "Failed parsing"
      failure
    OA.CompletionInvoked _ -> failure

  expected === result

verifyFailure :: List String -> Property
verifyFailure argList = withTests 1 $ property $ do
  let parseResult = OA.execParserPure prefs parserInfoArgs argList

  case parseResult of
    OA.Success _ -> failure
    OA.Failure _ -> pure ()
    OA.CompletionInvoked _ -> failure

prefs :: ParserPrefs
prefs = OA.prefs mempty

defCommand :: NESeq Text
defCommand = "command" :<|| Seq.empty

defArgs :: Maybe Args
defArgs = Just $ Args.defaultArgs defCommand

updateDefArgs ::
  forall a.
  Lens' Args (WithDisabled a) ->
  a ->
  Maybe Args
updateDefArgs l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % l

disableDefArgs ::
  forall a.
  Lens' Args (WithDisabled a) ->
  Maybe Args
disableDefArgs l = (l' .~ Disabled) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % l

updateDefCoreArgs ::
  forall a.
  Lens' CoreConfigArgs (WithDisabled a) ->
  a ->
  Maybe Args
updateDefCoreArgs l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % l

disableDefCoreArgs ::
  forall a.
  Lens' CoreConfigArgs (WithDisabled a) ->
  Maybe Args
disableDefCoreArgs l = (l' .~ Disabled) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % l
