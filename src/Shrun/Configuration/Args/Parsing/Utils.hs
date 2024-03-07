module Shrun.Configuration.Args.Parsing.Utils
  ( withDisableParser,
    withDisableParserHelp,
    mkHelp,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Shrun.Configuration.Data.ConfigPhase (WithDisable (Disabled, With))
import Shrun.Prelude

withDisableParser :: Parser a -> String -> Parser (WithDisable a)
withDisableParser mainParser name =
  withDisableParserHelp mainParser name ("Disables --" ++ name)

withDisableParserHelp :: Parser a -> String -> String -> Parser (WithDisable a)
withDisableParserHelp mainParser name helpTxt = do
  x <- mainParser
  y <- noParser
  pure
    $ if y
      then Disabled
      else With x
  where
    noParser =
      OA.flag
        False
        True
        ( mconcat
            [ OA.long $ "no-" ++ name,
              OA.internal,
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
