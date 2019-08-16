{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App ( runApp 
           , Env (..) ) where

import Reexport hiding ( ReaderT )
import qualified Parsing.Config as Config
import Updater ( Update (..) 
               , Message (..) 
               , Receiver (..) )
import qualified Logger
import qualified Messenger
import qualified Messenger.Impl.Telegram as Telegram
import Network.HTTP.Simple ( Proxy (..) )
import Control.Monad.IO.Unlift ( MonadUnliftIO )
import Control.Monad.Reader ( ReaderT (..) )
import qualified Data.Map as Map

data Env = Env { messenger :: Messenger.Handle
               , logger    :: Logger.Handle } 

data BotState = BotState { repeatCount :: Int }

newtype App a = 
    App { unApp :: ReaderT Env IO a }
    deriving ( Functor, Applicative 
             , Monad, MonadIO, MonadReader Env
             , MonadUnliftIO )

type Log = Logger.Handle
type IM = Messenger.Handle
type StateMap = Map.Map Updater.Receiver BotState

class Monad m => Has e m where
    get    :: m e
    getBy  :: (e -> a) -> m a

instance Has Env App where
    get    = ask
    getBy  = asks

instance Has Log App where
    get     = asks logger
    getBy f = (asks logger) >>= \l -> pure $ f l 

instance Has IM App where
    get     = asks messenger 
    getBy f = (asks messenger) >>= \i -> pure $ f i

class (Has Log m) => Logging m where
    logTrace   :: Text -> m ()
    logDebug   :: Text -> m ()
    logInfo    :: Text -> m ()
    logWarning :: Text -> m ()
    logError   :: Text -> m ()
    logFatal   :: Text -> m ()

instance Logging App where
    logTrace   = toLog Logger.Trace
    logDebug   = toLog Logger.Debug
    logInfo    = toLog Logger.Info
    logWarning = toLog Logger.Warning
    logError   = toLog Logger.Error
    logFatal   = toLog Logger.Fatal

class (Has IM m, MonadIO m) => WithMessenger m where
    getUpdates   :: m [Update]
    getChannel   :: m (Chan Update)
    sendMessage  :: Text -> Receiver -> m ()
    sendKeyboard :: Text -> Receiver -> [[String]] -> m ()
    getText      :: Message -> m Text

instance WithMessenger App where
    getUpdates = do
        handler <- get
        liftIO $ Messenger.getUpdates handler
    getChannel = do
        handler <- get
        liftIO $ Messenger.updatesChannel handler
    sendMessage t r = do
        handler <- get
        let message = Message t
        liftIO $ Messenger.send handler message r
    sendKeyboard t r kb = do
        handler <- get
        let message = Message t
        liftIO $ Messenger.keyboard handler kb (Just message) r
    getText (Message t) = pure t


class (MonadIO m) => WithState m where
    newMap      :: m (IORef StateMap)
    insertToMap :: IORef StateMap
                -> Updater.Receiver 
                -> BotState
                -> m (IORef StateMap)
    readFromMap :: IORef StateMap
                -> Updater.Receiver
                -> m (Maybe BotState)

instance WithState App where
    newMap = liftIO $ newIORef Map.empty
    insertToMap m k v = do
        m_ <- liftIO $ readIORef m
        let newM = Map.insert k v m_
        liftIO $ writeIORef m newM
        pure m
    readFromMap m k = do
        m_ <- liftIO $ readIORef m
        pure $ Map.lookup k m_

toLog ::
    ( Has Log m 
    , MonadIO m )
    => Logger.LogLevel
    -> Text
    -> m ()
toLog logLevel text = do
    loggerHandle <- get
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
runApp env = runReaderT ( unApp runBot ) env

runBot :: 
    ( MonadIO m 
    , WithMessenger m
    , Logging m 
    , WithState m)
    => m ()
runBot = do
    stateMap <- newMap
    forever $ do
        channel <- getChannel
        update  <- liftIO $ readChan channel
        let receiver_ = receiver update
        textMessage <- getText $ message update
        case textMessage of
            "/help" -> sendKeyboard "Help text" receiver_ 
                            [ ["1","2"] ]
            otherwise -> do
                    maybeState <- readFromMap stateMap receiver_
                    let count = 
                            case maybeState of
                                Nothing -> 2
                                Just st -> repeatCount st
                    sequence_ 
                        $ take count 
                        $ repeat 
                        $ sendMessage textMessage receiver_
                    logTrace textMessage
        return ()



