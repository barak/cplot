{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module App.Types where

import           Chart.Types                 (Chart)
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Aeson.Types            (typeMismatch)
import           Data.ByteString             (ByteString)
import           Data.Default
import           Data.HashMap.Lazy           (HashMap)
import qualified Data.HashMap.Lazy           as Map
import           Data.IORef                  (IORef)
import           Data.Text                   (Text)
import           Data.Yaml                   (FromJSON (..), (.:))
import qualified Data.Yaml                   as Y
import           GHC.Int                     (Int32)
import           Options
import           Text.RawString.QQ


newtype App a = App { unApp :: ReaderT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadReader AppEnv, MonadThrow)

instance MonadBaseControl IO App where
  type StM App a = a
  liftBaseWith f = App $ liftBaseWith $ \q -> f (q . unApp)
  restoreM       = App . restoreM

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
  { _fps          :: Int
  -- ^ Rate at which the chart view updates
  , _drainRate    :: Int
  -- ^ Rate (in μs) at which each charts buffers are drained
  , _windowWidth  :: Int32
  -- ^ Default width (in pixels) of window
  , _windowHeight :: Int32
  -- ^ Default height (in pixels) of window
  }

-- | 'Global' application state with components you can change (safely)
data AppState = AppState
  { _chartRefs :: HashMap Text (IORef Chart) }

makeClassy ''AppEnv
makeClassy ''AppConfig
makeClassy ''AppState

instance HasAppState   AppEnv where appState   = state
instance HasAppConfig  AppEnv where appConfig  = config
instance HasAppOptions AppEnv where appOptions = options

instance FromJSON AppConfig where
  parseJSON (Y.Object v) =
    AppConfig <$> v .: "FPS"
              <*> v .: "drain-rate"
              <*> v .: "default-width"
              <*> v .: "default-height"
  parseJSON invalid = typeMismatch "AppConfig" invalid

--------------------------------------------------------------------------------
-- DEFAULT INSTANCES

-- it's worth iterating on these values
defaultAppConfig :: ByteString
defaultAppConfig = [r|
FPS:            24
drain-rate:     1000
default-width:  800
default-height: 500
|]

instance Default AppConfig where
  def = AppConfig
    { _fps          = 24
    , _drainRate    = 1000
    , _windowWidth  = 800
    , _windowHeight = 500
    }

-- doesn't really make sense to have a default instance for this
instance Default AppState where
  def = AppState
    { _chartRefs = Map.empty }
