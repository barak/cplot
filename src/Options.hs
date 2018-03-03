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

import           Control.Arrow       (left)
import           Control.Lens
import           Data.HashMap.Lazy   (HashMap)
import qualified Data.HashMap.Lazy   as Map
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Options.Applicative

import qualified Parser.Options      as MP
import qualified Text.Megaparsec     as MP

import           Chart

data AppOptions = AppOptions
  { _initialCharts :: HashMap Text Chart }

makeClassy ''AppOptions


parseArgs :: IO AppOptions
parseArgs = execParser (info (helper <*> parseOptions)
                             (header "cplot"))

parseOptions :: Parser AppOptions
parseOptions = AppOptions . toChartMap <$> chartsParser
  where
    chartsParser =
      some $ option chartReader $
        long "chart"
     <> short 'c'
     <> help "[chart name] [subchart label] [line|scatter] [subchart label] ..."

    toChartMap cs = Map.fromList [ (chart ^. title, chart) | chart <- cs ]

-- | Optparse specific ChartType parser
chartReader :: ReadM Chart
chartReader = parsecReadM MP.parseChart

-- | Transforms megaparsec parsers into optparse ReadM parsers
parsecReadM :: MP.Parser a -> ReadM a
parsecReadM p = eitherReader (left MP.parseErrorPretty . MP.parse p "" . T.pack)
