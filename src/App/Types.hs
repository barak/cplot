{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module App.Types where

import           Control.Concurrent.MVar
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson.Types            (typeMismatch)
import           Data.ByteString             (ByteString)
import           Data.Default
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.Text                   (Text)
import           Data.Word                   (Word32)
import           Data.Yaml                   (FromJSON (..), (.:))
import qualified Data.Yaml                   as Y
import           GHC.Int                     (Int32)
import           Options
import           Text.RawString.QQ

import           Chart.Types                 (Chart)


newtype App a = App { unApp :: ReaderT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadThrow)

-- | Core application data type
data AppEnv = AppEnv
  { _options :: AppOptions
  -- ^ Command line options
  , _config  :: AppConfig
  -- ^ Global app configuration
  , _state   :: AppState
  -- ^ Global application state
  }

-- | 'Global' config set at runtime by a .conf or .yaml file
data AppConfig = AppConfig
  { _fps          :: Word32
  -- ^ Rate at which the chart view updates
  , _flushRate    :: Int
  -- ^ Rate (in μs) at which each charts buffers are flushed
  , _windowWidth  :: Int32
  -- ^ Default width (in pixels) of window
  , _windowHeight :: Int32
  -- ^ Default height (in pixels) of window
  , _lineConfig   :: LineConfig
  -- ^ line chart specific configuration
  }

-- | 'Global' application state with components you can change (safely)
data AppState = AppState
  { _chartRefs   :: HashMap Text (MVar Chart, MVar ()) }

instance FromJSON AppConfig where
  parseJSON (Y.Object v) =
    AppConfig <$> v .: "FPS"
              <*> v .: "flush-rate"
              <*> v .: "default-width"
              <*> v .: "default-height"
              <*> v .: "line-config"
  parseJSON invalid = typeMismatch "AppConfig" invalid

newtype LineConfig = LineConfig
  { _cycleAfter :: Int }

instance FromJSON LineConfig where
  parseJSON (Y.Object v) =
    LineConfig <$> v .: "max-points"
  parseJSON invalid = typeMismatch "LineConfig" invalid

--------------------------------------------------------------------------------
-- DEFAULTS

-- it's worth iterating on these values
defaultAppConfig :: ByteString
defaultAppConfig = [r|# CPLOT DEFAULT CONFIG

FPS:            24
flush-rate:     1000
default-width:  800
default-height: 500

line-config:
  max-points: 1000
|]

instance Default AppConfig where
  def = AppConfig
    { _fps          = 24
    , _flushRate    = 1000
    , _windowWidth  = 800
    , _windowHeight = 500
    , _lineConfig   = def
    }

instance Default LineConfig where
  def = LineConfig { _cycleAfter = 1000 }

instance Default AppState where
  def = AppState
    { _chartRefs   = Map.empty
    }

--------------------------------------------------------------------------------
-- GENERATE LENSES

makeClassy ''AppEnv
makeClassy ''AppConfig
makeClassy ''AppState
makeLenses ''LineConfig

instance HasAppState   AppEnv where appState   = state
instance HasAppConfig  AppEnv where appConfig  = config
instance HasAppOptions AppEnv where appOptions = options

-- Constraint synonyms for extra laziness
type HasState   r m = (MonadReader r m, HasAppState r)
type HasConfig  r m = (MonadReader r m, HasAppConfig r)
type HasOptions r m = (MonadReader r m, HasAppOptions r)
