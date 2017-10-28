{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module App.Types where

import           Chart.Types          (Chart)
import           Control.Lens
import           Control.Monad.Reader
import           Data.Default
import           Data.IORef           (IORef)
import           Options


newtype App a = App { unApp :: ReaderT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

-- | Core application data type
data AppEnv = AppEnv
  { _appOptions :: AppOptions       -- ^ Command line options
  , _appConfig  :: AppConfig        -- ^ Global app configuration
  , _appState   :: AppState         -- ^ Global application state
  , _appLogger  :: String -> IO ()  -- ^ Logging function
  }

-- | 'Global' config set at runtime by a .conf or .yaml file
data AppConfig = AppConfig

-- | 'Global' application state with components you can change (safely)
data AppState = AppState
  { _chartRefs :: [IORef Chart] }

makeFieldsNoPrefix ''AppEnv
makeLenses ''AppConfig
makeLenses ''AppState

--------------------------------------------------------------------------------
-- DEFAULT INSTANCES

instance Default AppConfig where
  def = AppConfig

instance Default AppState where
  def = AppState { _chartRefs = [] }
