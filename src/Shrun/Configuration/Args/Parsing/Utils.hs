module Shrun.Configuration.Args.Parsing.Utils
  ( withDisabledParser,
    withDisabledParserOpts,
    mkHelp,
    mkHelpNoLine,
    autoStripUnderscores,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Options.Applicative.Builder (FlagFields, Mod, ReadM)
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled
      ( Disabled,
        With,
        Without
      ),
  )
import Shrun.Prelude
import Shrun.Utils qualified as ShrunUtils
import Text.Read (Read)

-- | Adds a '--no-x' switch to the parser.
withDisabledParser ::
  -- | Main parser.
  Parser (Maybe a) ->
  -- | Name for this option, to be used in disabled switch name.
  String ->
  Parser (WithDisabled a)
withDisabledParser mainParser name =
  withDisabledParserOpts opts mainParser name
  where
    helpTxt = "Disables --" ++ name ++ "."
    opts = mkHelp helpTxt

-- | Like 'withDisabledParser', except it also takes an arg for the disabled
-- switch options.
withDisabledParserOpts ::
  -- | Disabled switch options.
  Mod FlagFields Bool ->
  -- | Main parser
  Parser (Maybe a) ->
  -- | Name for this option, to be used in disabled switch name.
  String ->
  Parser (WithDisabled a)
withDisabledParserOpts disabledOpts mainParser name = do
  mx <- mainParser
  y <- noParser
  pure
    $ if y
      then Disabled
      else maybe Without With mx
  where
    noParser =
      OA.flag
        False
        True
        ( mconcat
            [ OA.long $ "no-" ++ name,
              OA.hidden,
              disabledOpts
            ]
        )

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
