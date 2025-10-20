module Shrun.Configuration.Args.Parsing.Utils
  ( mkHelp,
    mkHelpNoLine,

    -- * Disabled parser
    mWithDisabledParser,

    -- * Switch parser
    switchParser,
    switchParserNoLine,
    switchParserOpts,

    -- * Misc
    autoStripUnderscores,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Options.Applicative.Builder (Mod, ReadM)
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
mWithDisabledParser rdr opts mv = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          reader
          (mconcat $ OA.metavar metavar : opts)

    metavar = "(" <> mv <> " | off)"

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
