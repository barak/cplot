{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Options
  ( AppOptions
  , parseArgs

  -- AppOptions lenses
  , chartTypes
  ) where

import           Chart.Types          (ChartType (..))
import           Control.Arrow        (left)
import           Control.Lens
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Options.Applicative
import qualified Text.Megaparsec      as MP
import qualified Text.Megaparsec.Char as MP


data AppOptions = AppOptions
  { _chartTypes :: [[ChartType]] }

makeLenses ''AppOptions

--------------------------------------------------------------------------------
-- OPTIONS PARSER

type MParser = MP.Parsec Void T.Text

parseArgs :: IO AppOptions
parseArgs = execParser (info (helper <*> parseOptions)
                             (header "cplot"))

parseOptions :: Parser AppOptions
parseOptions = AppOptions <$> chartTypes'
  where
    chartTypes' =
      some $ option chartReader $
        long "chart"
     <> short 'c'
     <> help "line/scatter (up to four may be specified)"

-- | Optparse specific ChartType parser
chartReader :: ReadM [ChartType]
chartReader = parsecReadM (some parseChartType)

-- | Transforms megaparsec parsers into optparse ReadM parsers
parsecReadM :: MParser a -> ReadM a
parsecReadM p = eitherReader (left MP.parseErrorPretty . MP.parse p "" . T.pack)

-- | megaparsec parser for ChartType
-- TODO: maybe write some TH to autogenerate this parser?
parseChartType :: MParser ChartType
parseChartType = (MP.string "line"    >> return Line)
             <|> (MP.string "scatter" >> return Scatter)
             <|> (MP.string "series"  >> return TimeSeries)
