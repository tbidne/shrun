{-# LANGUAGE OverloadedLists #-}

module Shrun.Configuration.Args.Parsing.Utils
  ( mkHelp,
    mkHelpNoLine,

    -- * Disabled parser
    withDisabledParser,
    mWithDisabledParser,
    withDisabledParserNoOpts,

    -- * Switch parser
    switchParser,
    switchParserNoLine,
    switchParserOpts,

    -- * Completers
    fileCompleter,
    fileCompleterSuffix,

    -- * Misc
    autoStripUnderscores,
  )
where

import Data.Either (fromRight)
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Effects.FileSystem.PathReader qualified as PR
import Effects.System.Process qualified as P
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Options.Applicative.Builder (Mod, ReadM)
import Options.Applicative.Builder.Completer (Completer)
import Options.Applicative.Builder.Completer qualified as Completer
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Prelude
import Shrun.Utils qualified as ShrunUtils
import Text.Read (Read)

-- Looks a bit convoluted, but this gets us what we want:
-- 1. lines aligned (paragraph)
-- 2. linebreak at the end (fmap hardline)
mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

mkHelpNoLine :: String -> OA.Mod f a
mkHelpNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . Chunk.paragraph

-- | Reads 'Text', strips underscores, then uses the Read class. This is
-- essentially 'auto' but removes underscores. This is used for nicer
-- numeric values e.g. allowing parsing "1_000_000" as a Num.
autoStripUnderscores :: (Read a) => ReadM a
autoStripUnderscores = OA.str >>= ShrunUtils.readStripUnderscores

-- | Constructs a parser for (Maybe (WithDisabled a)).
mWithDisabledParser ::
  -- | Reader for a.
  ReadM a ->
  -- | Modifier list e.g. option name, help text.
  List (Mod OA.OptionFields (WithDisabled a)) ->
  -- | Metavar string.
  String ->
  Parser (Maybe (WithDisabled a))
mWithDisabledParser rdr opts = OA.optional . withDisabledParser rdr opts

-- | Constructs a parser for (WithDisabled a).
withDisabledParser ::
  -- | Reader for a.
  ReadM a ->
  -- | Modifier list e.g. option name, help text.
  List (Mod OA.OptionFields (WithDisabled a)) ->
  -- | Metavar string.
  String ->
  Parser (WithDisabled a)
withDisabledParser rdr opts mv =
  withDisabledParserNoOpts rdr opts'
  where
    metavar = "(" <> mv <> " | off)"

    opts' =
      OA.completeWith ["off"]
        : OA.metavar metavar
        : opts

-- | Constructs a parser for (Maybe (WithDisabled a)).
withDisabledParserNoOpts ::
  -- | Reader for a.
  ReadM a ->
  -- | Modifier list e.g. option name, help text.
  List (Mod OA.OptionFields (WithDisabled a)) ->
  Parser (WithDisabled a)
withDisabledParserNoOpts rdr opts = mainParser
  where
    mainParser =
      OA.option
        reader
        (mconcat opts)

    reader = do
      txt <- OA.str
      WD.disabledParser txt rdr

switchParser :: (Bool -> a) -> String -> String -> Parser (Maybe a)
switchParser = switchParserHelper mkHelp mempty

switchParserOpts :: Mod OA.OptionFields Bool -> (Bool -> a) -> String -> String -> Parser (Maybe a)
switchParserOpts = switchParserHelper mkHelp

switchParserNoLine :: (Bool -> a) -> String -> String -> Parser (Maybe a)
switchParserNoLine = switchParserHelper mkHelpNoLine mempty

switchParserHelper ::
  -- | Help function, determines final newlines behavior.
  (String -> Mod OA.OptionFields Bool) ->
  -- | Additional options.
  Mod OA.OptionFields Bool ->
  -- | Type constructor.
  (Bool -> a) ->
  -- | Option name.
  String ->
  -- | Help text.
  String ->
  Parser (Maybe a)
switchParserHelper mkHelpFn opts cons name helpTxt = fmap cons <$> mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          readBool
          ( mconcat
              [ opts,
                OA.long name,
                OA.metavar "(on | off)",
                OA.completeWith ["on", "off"],
                mkHelpFn helpTxt
              ]
          )

    readBool =
      OA.str @Text >>= \case
        "on" -> pure True
        "off" -> pure False
        other ->
          fail
            $ mconcat
              [ "Expected (on | off), received: '",
                unpack other,
                "'"
              ]

-- | File completer that tries compgen and gracefully falls back to
-- ordinary IO.
fileCompleter :: Completer
fileCompleter = bashCompleter "file" <> actionFileFallback

-- | 'fileCompleter' that filters on the given suffix.
fileCompleterSuffix :: String -> Completer
fileCompleterSuffix sfx =
  bashCompleter compgenFilter
    <> actionFileFallbackFilter strFilter
  where
    compgenFilter =
      mconcat
        [ "file -X '!*",
          sfx,
          "'"
        ]

    strFilter = L.isSuffixOf sfx

-- | Haskell IO completer fallback for 'bashCompleter "file"' i.e. does not
-- rely on compgen.
actionFileFallback :: Completer
actionFileFallback = actionFileFallbackFilter (const True)

-- | 'actionFileFallback' that additionally applies the given filter.
actionFileFallbackFilter :: (String -> Bool) -> Completer
actionFileFallbackFilter pred = Completer.mkCompleter $ \word -> do
  eFiles <- tryMySync $ do
    cwd <- PR.getCurrentDirectory
    PR.listDirectory cwd

  let files = fromRight [] eFiles
  fmap toList $ flip foldMapA files $ \p -> do
    isFile <-
      tryMySync (PR.doesFileExist p) <&> \case
        Left _ -> False
        Right b -> b

    let pStr = decodeLenient p
        matchesPat = word `L.isPrefixOf` pStr
    pure
      $ if isFile && matchesPat && pred pStr
        then Seq.singleton pStr
        else Empty

-- | This is like optparse-applicative's bashComplete with one exception:
-- This swallows stderr intentionally, to avoid cluttering the output.
-- If compgen doesn't exist (e.g. nix develop), then OA will fail and print
-- stderr, which is ugly.
--
-- By using readCreateProcessWithExitCode, we can swallow stderr. This
-- also means we do not have to us try/catch or manually pass in 'bash -c',
-- like OA.
bashCompleter :: String -> Completer
bashCompleter action = Completer.mkCompleter $ \word -> do
  let cmd = L.unwords ["compgen", "-A", action, "--", Completer.requote word]
  (ec, out, _err) <- P.readCreateProcessWithExitCode (P.shell cmd) ""
  pure $ case ec of
    ExitFailure _ -> []
    ExitSuccess -> L.lines out
