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

execParserUnit :: List String -> ParserResult (Args NotifyEnv)
execParserUnit = OA.execParserPure prefs (parserInfoArgs [])

verifyResult :: List String -> Maybe (Args NotifyEnv) -> Property
verifyResult argList =
  withTests 1
    . property
    . verifyResultT argList

verifyResultT :: [String] -> Maybe (Args NotifyEnv) -> PropertyT IO ()
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
          errTxt = pack errStr
      annotate $ textEq expected errTxt
      annotate (unpack expected)
      annotate errStr
      assert (expected `T.isPrefixOf` errTxt)
    OA.CompletionInvoked _ -> failure

textEq :: Text -> Text -> String
textEq t1 t2 = go (unpack t1) (unpack t2)
  where
    go [] [] = ""
    go xs@(_ : _) [] = "LHS non-empty: " <> xs
    go [] ys@(_ : _) = "RHS non-empty: " <> ys
    go (x : xs) (y : ys)
      | x == y = go xs ys
      | otherwise = '\'' : x : "' /= '" <> [y, '\'']

prefs :: ParserPrefs
prefs = OA.prefs mempty

defCommand :: NESeq Text
defCommand = "command" :<|| Seq.empty

defArgs :: Maybe (Args NotifyEnv)
defArgs = Just $ Args.defaultArgs defCommand

updateDefArgs ::
  forall a.
  Lens' (Args NotifyEnv) (Maybe a) ->
  a ->
  Maybe (Args NotifyEnv)
updateDefArgs l x = (l' ?~ x) defArgs
  where
    l' :: AffineTraversal' (Maybe (Args NotifyEnv)) (Maybe a)
    l' = _Just % l

updateDefArgsWD ::
  forall a.
  Lens' (Args NotifyEnv) (WithDisabled a) ->
  a ->
  Maybe (Args NotifyEnv)
updateDefArgsWD l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe (Args NotifyEnv)) (WithDisabled a)
    l' = _Just % l

disableDefArgs ::
  forall a.
  Lens' (Args NotifyEnv) (Maybe (WithDisabled a)) ->
  Maybe (Args NotifyEnv)
disableDefArgs l = (l' ?~ Disabled) defArgs
  where
    l' :: AffineTraversal' (Maybe (Args NotifyEnv)) (Maybe (WithDisabled a))
    l' = _Just % l

updateDefCoreArgs ::
  forall a.
  Lens' (CoreConfigArgs NotifyEnv) (Maybe a) ->
  a ->
  Maybe (Args NotifyEnv)
updateDefCoreArgs l x = (l' ?~ x) defArgs
  where
    l' :: AffineTraversal' (Maybe (Args NotifyEnv)) (Maybe a)
    l' = _Just % #coreConfig % l

updateDefCoreArgsWD ::
  forall a.
  Lens' (CoreConfigArgs NotifyEnv) (WithDisabled a) ->
  a ->
  Maybe (Args NotifyEnv)
updateDefCoreArgsWD l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe (Args NotifyEnv)) (WithDisabled a)
    l' = _Just % #coreConfig % l

disableDefCoreArgs ::
  forall a.
  Lens' (CoreConfigArgs NotifyEnv) (Maybe (WithDisabled a)) ->
  Maybe (Args NotifyEnv)
disableDefCoreArgs l = (l' ?~ Disabled) defArgs
  where
    l' :: AffineTraversal' (Maybe (Args NotifyEnv)) (Maybe (WithDisabled a))
    l' = _Just % #coreConfig % l
