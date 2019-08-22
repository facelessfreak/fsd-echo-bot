{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App ( runApp 
           , Env (..) ) where

import qualified  Parsing.Config          as Config
import qualified  Messenger.Impl.Telegram as Telegram
import qualified  Data.Map                as Map
import qualified  Logger
import qualified  Messenger

import            Reexport hiding         ( ReaderT )
import            Network.HTTP.Simple     ( Proxy    (..) )
import            Control.Monad.IO.Unlift ( MonadUnliftIO )
import            Control.Monad.Reader    ( ReaderT  (..) )
import            Updater                 ( Update   (..) 
                                          , Message  (..) 
                                          , Receiver (..) )

data Env = Env { messenger :: Messenger.Handle
               , logger    :: Logger.Handle } 

data BotMode = Normal | InsertCount

data BotState = BotState { repeatCount :: Int 
                         , botMode     :: BotMode }

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
  getUpdates      :: m [Update]
  getChannel      :: m (Chan Update)
  sendMessage     :: Text -> Receiver -> m ()
  sendKeyboard    :: Text -> Receiver -> [[String]] -> m ()
  removeKeyboard  :: Text -> Receiver -> m ()
  getText         :: Message -> m Text

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
    liftIO $ Messenger.keyboard handler (Messenger.CreateKbd kb) (Just message) r
  removeKeyboard t r = do
    handler <- get
    let message = Message t
    liftIO $ Messenger.keyboard handler (Messenger.RemoveKbd ) (Just message) r
  getText (Message t) = pure t


class (MonadIO m) => WithState m where
  newMap      :: m (IORef StateMap)
  insertToMap :: IORef StateMap
              -> Updater.Receiver 
              -> BotState
              -> m ()
  readFromMap :: IORef StateMap
              -> Updater.Receiver
              -> m (Maybe BotState)

instance WithState App where
  newMap = liftIO $ newIORef Map.empty
  insertToMap m k v = do
    m_ <- liftIO $ readIORef m
    let newM = Map.insert k v m_
    liftIO $ writeIORef m newM
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
  channel  <- getChannel
  forever $ do
    update  <- liftIO $ readChan channel
    let receiver_ = receiver update
    maybeState  <- readFromMap stateMap receiver_
    let currentState = 
          case maybeState of
            Nothing -> getDefaultBotState
            Just st -> st
    textMessage <- getText $ message update
    logTrace textMessage
    case botMode currentState of
      Normal      -> do
        case textMessage of
          "/help"     -> do
            sendMessage "Placeholder for help message" receiver_
          "/repeat"   -> do 
            sendKeyboard "Placeholder for repeat message" receiver_
              [ ["1", "2"] 
              , ["3", "4"]
              , ["5", "Cancel"] ]
            insertToMap stateMap receiver_ $ 
              currentState { botMode = InsertCount }
          otherwise   -> do 
            let count = repeatCount currentState
            sequence_
              $ take count
              $ repeat
              $ sendMessage textMessage receiver_
      InsertCount -> do
        let newCount =
              case textMessage of
                "1"         -> 1
                "2"         -> 2
                "3"         -> 3
                "4"         -> 4
                "5"         -> 5
                otherwise   -> repeatCount currentState
        removeKeyboard 
          (tPack $ "Current repeat count is " ++ (show newCount))
          receiver_
        insertToMap stateMap receiver_ $
          currentState { repeatCount = newCount 
                       , botMode     = Normal }

getDefaultBotState :: BotState
getDefaultBotState = 
  BotState { repeatCount = 1 
           , botMode     = Normal }

