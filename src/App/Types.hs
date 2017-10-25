{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Should use the default class, messy to expose this much of each data type.
-- At least smart constructors would be more manageable
module App.Types
  ( App(..)
  , AppEnv(AppEnv)
  , AppConfig(..)
  , AppState(AppState)
  , HasAppOptions
  , HasAppConfig
  , HasAppState
  , HasAppLogger

  -- AppEnv lenses
  , appOptions
  , appConfig
  , appState
  , appLogger

  -- AppState lenses
  , charts
  ) where

import           Chart.Types          (Chart)
import           Control.Lens
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
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
  { _charts :: [Chart] }

makeFieldsNoPrefix ''AppEnv
makeLenses ''AppConfig
makeLenses ''AppState
