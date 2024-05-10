module Shrun.Configuration.Args.Parsing.Utils
  ( withDisabledParser,
    withDisabledParserHelp,
    mkHelp,
    autoStripUnderscores,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Options.Applicative.Builder (ReadM)
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

withDisabledParser :: Parser (Maybe a) -> String -> Parser (WithDisabled a)
withDisabledParser mainParser name =
  withDisabledParserHelp mainParser name ("Disables --" ++ name ++ ".")

withDisabledParserHelp ::
  Parser (Maybe a) ->
  String ->
  String ->
  Parser (WithDisabled a)
withDisabledParserHelp mainParser name helpTxt = do
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
              mkHelp helpTxt
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

-- | Reads 'Text', strips underscores, then uses the Read class. This is
-- essentially 'auto' but removes underscores. This is used for nicer
-- numeric values e.g. allowing parsing "1_000_000" as a Num.
autoStripUnderscores :: (Read a) => ReadM a
autoStripUnderscores = OA.str >>= ShrunUtils.readStripUnderscores
