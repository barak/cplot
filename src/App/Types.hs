{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module App.Types where

import           Chart.Types            (Chart)
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Reader
import           Data.Default
import           Data.HashMap.Lazy      (HashMap)
import qualified Data.HashMap.Lazy      as Map
import           Data.IORef             (IORef)
import           Data.Text              (Text)
import           Options


newtype App a = App { unApp :: ReaderT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadThrow)

-- | Core application data type
data AppEnv = AppEnv
  { _options :: AppOptions  -- ^ Command line options
  , _config  :: AppConfig   -- ^ Global app configuration
  , _state   :: AppState    -- ^ Global application state
  }

-- | 'Global' config set at runtime by a .conf or .yaml file
data AppConfig = AppConfig

-- | 'Global' application state with components you can change (safely)
data AppState = AppState
  { _chartRefs :: HashMap Text (IORef Chart) }

makeClassy ''AppEnv
makeClassy ''AppConfig
makeClassy ''AppState

instance HasAppState   AppEnv where appState   = state
instance HasAppConfig  AppEnv where appConfig  = config
instance HasAppOptions AppEnv where appOptions = options

--------------------------------------------------------------------------------
-- DEFAULT INSTANCES

instance Default AppConfig where
  def = AppConfig

instance Default AppState where
  def = AppState { _chartRefs = Map.empty }
