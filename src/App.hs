{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module App ( runApp 
           , Env (..) ) where

import Reexport
import qualified Parsing.Config as Config
import qualified Logger

data Env = Env { config :: [Config.Messenger]
               , logger :: Logger.Handle } 

newtype App a = 
    App { unApp :: ReaderT Env IO a }
    deriving ( Functor, Applicative 
             , Monad, MonadIO, MonadReader Env )

class Monad m => HasEnv m where
    getEnv :: m Env

instance HasEnv App where
    getEnv = ask

class HasEnv m => HasLog m where
    logTrace   :: Text -> m ()
    logDebug   :: Text -> m ()
    logInfo    :: Text -> m ()
    logWarning :: Text -> m ()
    logError   :: Text -> m ()
    logFatal   :: Text -> m ()

instance HasLog App where
    logTrace   = toLog Logger.Trace
    logDebug   = toLog Logger.Debug
    logInfo    = toLog Logger.Info
    logWarning = toLog Logger.Warning
    logError   = toLog Logger.Error
    logFatal   = toLog Logger.Fatal

toLog ::
    ( HasEnv m 
    , MonadIO m )
    => Logger.LogLevel
    -> Text
    -> m ()
toLog logLevel text = do
    env <- getEnv
    let loggerHandle = logger env
    liftIO $
        case logLevel of
        Logger.Trace   -> Logger.logTrace   loggerHandle text
        Logger.Debug   -> Logger.logDebug   loggerHandle text
        Logger.Info    -> Logger.logInfo    loggerHandle text
        Logger.Warning -> Logger.logWarning loggerHandle text
        Logger.Error   -> Logger.logError   loggerHandle text
        Logger.Fatal   -> Logger.logFatal   loggerHandle text


runApp
    :: Env
    -> IO ()
runApp env =
    runReaderT ( unApp runBot ) env


runBot :: 
    ( MonadIO m 
    , HasEnv m 
    , HasLog m )
    => m ()
runBot = do
    env <- getEnv
    liftIO $ 
        mapM (\messenger ->
            if (Config.name messenger) == "slack"
            then pure ()
            else do
                liftIO $ do
                    forkIO $ do
                        undefined
                return ()
            )
        (config env ) 
    undefined

