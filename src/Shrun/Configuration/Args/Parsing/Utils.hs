{-# LANGUAGE OverloadedLists #-}

module Shrun.Configuration.Args.Parsing.Utils
  ( -- * Simple help
    mkHelp,
    mkHelpNoLine,

    -- * Disabled parser
    withDisabledParser,
    mWithDisabledParser,
    withDisabledParserNoOpts,

    -- * Switch parser
    switchParser,
    switchParserNoLine,
    switchParserOpts,

    -- * Misc
    autoStripUnderscores,
    itemize,
    itemizeNoLine,
    itemizeHelper,
    toChunk,
    toMDoc,
  )
where

import Options.Applicative (OptionFields, Parser)
import Options.Applicative qualified as OA
import Options.Applicative.Builder (Mod, ReadM)
import Options.Applicative.Help (Doc)
import Options.Applicative.Help.Chunk (Chunk (Chunk))
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
    . toMDoc

mkHelpNoLine :: String -> OA.Mod f a
mkHelpNoLine = OA.helpDoc . toMDoc

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

toMDoc :: String -> Maybe Doc
toMDoc = Chunk.unChunk . Chunk.paragraph

-- | Make an itemized list e.g.
--
-- @
--   itemize [intro, l1, l2, l2]
--
--   ==> intro
--
--       - l1
--       - l2
--       - l3
-- @
itemize :: NESeq String -> Mod OptionFields a
itemize =
  OA.helpDoc
    . Chunk.unChunk
    . fmap (<> Pretty.line)
    . itemizeHelper

-- | 'itemize' that does not append a trailing newline. Useful for the last
-- option in a group, as groups already start a newline.
itemizeNoLine :: NESeq String -> Mod OptionFields a
itemizeNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . itemizeHelper

itemizeHelper :: NESeq String -> Chunk Doc
itemizeHelper (intro :<|| ds) =
  Chunk.vcatChunks
    $ toList
      ( Chunk.paragraph intro
          :<| toChunk Pretty.softline
          :<| (toItem <$> ds)
      )
  where
    toItem d =
      fmap (Pretty.nest 2)
        . Chunk.paragraph
        $ ("- " <> d)

toChunk :: a -> Chunk a
toChunk = Chunk . Just
