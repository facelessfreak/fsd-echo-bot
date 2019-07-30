{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main_old where

import qualified Messenger                          as Messenger 
import qualified Messenger.Impl.Telegram            as Telegram
import qualified Messenger.Impl.Slack               as Slack
import qualified HTTP                               as HTTP
import qualified HTTP.Impl.Conduit                  as Conduit
import qualified Logger                             as Logger
import qualified Logger.Impl.Console                as Console
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.State 
import           Types
import           Control.Concurrent ( forkIO
                                    , ThreadId
                                    , MVar
                                    , Chan
                                    , newChan)
import qualified Data.Text                          as T
import qualified EchoBot.Impl.IM                    as EchoBot

getTelegramConfig 
    :: AppConfig 
    -> Telegram.Config
getTelegramConfig 
    = undefined

getSlackConfig 
    :: AppConfig 
    -> Slack.Config
getSlackConfig 
    = undefined


data MessengerState
    = MessengerState 
        { msCount     :: !(Int) 
        , msID        :: !(Maybe Messenger.Message)
        , msChat      :: !(Maybe Messenger.Chat)
        , msHandle    :: !(Messenger.Handle)
        }

setCount
    :: Int 
    -> MessengerState
    -> MessengerState
setCount c s
    = s { msCount = c }


data MessengerTask
    = SendMessage T.Text Messenger.Chat
    | SetRepeatCount Int

createTask
    :: Messenger.Update 
    -> MessengerTask
createTask = undefined

completeTask
    :: MessengerTask
    -> AppMessenger ()
completeTask t = AppMessenger $
    case t of
    SendMessage m ch -> do
        c <- gets msCount
        h <- gets msHandle
        lift $ sequence $ take c $ repeat $ Messenger.sendMessage h ch $ Messenger.ContentText m
        return ()
    SetRepeatCount c -> modify $ setCount c

newtype AppMessenger a = AppMessenger { unAppMessenger :: StateT MessengerState IO a }

runWithState
    :: MessengerState
    -> AppMessenger ()
    -> IO MessengerState
runWithState s app = do
    (_, s2) <- runStateT (unAppMessenger app) s
    return s2

runMessenger
    :: Messenger.Handle 
    -> AppMessenger ()
runMessenger h = AppMessenger $ do
    chat    <- gets msChat
    message <- gets msID
    updates <- lift $ Messenger.getUpdates h message chat
    let tasks = map createTask updates
    mapM (\t -> do
        s <- get
        lift $ runWithState s $ completeTask t 
        ) tasks
    return ()

initialState h = MessengerState 1 Nothing Nothing h

startMessenger :: Messenger.Handle -> IO MessengerState
startMessenger h = do
    runWithState (initialState h) $ runMessenger h

main :: IO ()
main = do
    logChan <- newChan
    commandChan <- newChan
    config <- undefined
    let telegramConfig  = getTelegramConfig config
    let slackConfig     = getSlackConfig config
    forkIO $ Telegram.withHandle telegramConfig $ (\h -> startMessenger h >> return ())
    return ()
