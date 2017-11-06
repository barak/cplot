{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Options
  ( AppOptions
  , HasAppOptions
  , parseArgs

  -- AppOptions lenses
  , appOptions
  , initialCharts
  ) where

import           Chart               (Chart)
import           Control.Arrow       (left)
import           Control.Lens
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           Options.Applicative

import qualified Parser.Generic      as MP
import qualified Parser.Options      as MP
import qualified Text.Megaparsec     as MP


data AppOptions = AppOptions
  { _initialCharts :: [Chart] }

makeClassy ''AppOptions


parseArgs :: IO AppOptions
parseArgs = execParser (info (helper <*> parseOptions)
                             (header "cplot"))

parseOptions :: Parser AppOptions
parseOptions = AppOptions <$> charts
  where
    charts =
      some $ option chartReader $
        long "chart"
     <> short 'c'
     <> help "[chart name] [subchart label] [line|scatter] [subchart label] ..."

-- | Optparse specific ChartType parser
chartReader :: ReadM Chart
chartReader = parsecReadM MP.parseChart

-- | Transforms megaparsec parsers into optparse ReadM parsers
parsecReadM :: MP.Parser a -> ReadM a
parsecReadM p = eitherReader (left MP.parseErrorPretty . MP.parse p "" . T.pack)
