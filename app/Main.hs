{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

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
                                    , ThreadId)

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


mainApp :: ( Configuration AppConfig m ) 
           => m ()
mainApp = do
    config  <- getConfig
    return ()
--  let slkConfig   = getSlackConfig config
--  fork . startTelegram $ tgConfig
--  fork . startSlack    $ slkConfig
--  return ()

data MessengerState
    = MessengerState 
        { msCount :: !(Int) 
        , msID    :: !(Maybe Messenger.MessageID)}

setCount
    :: Int 
    -> MessengerState
    -> MessengerState
setCount c s
    = s { msCount = c }


main :: IO ()
main = do
    config <- undefined
    let telegramConfig  = getTelegramConfig config
    let slackConfig     = getSlackConfig config
    forkIO $
        Telegram.withHandle telegramConfig $ \h -> do
            runStateT ( do 
                updates <- lift $ Messenger.getUpdates h Nothing 
                handleResults <- lift $ 
                    mapM ( (Messenger.handleContent h) . Messenger.muContent) updates

                mapM (\h -> case h of
                            Messenger.RepeatCount c -> modify $ setCount c
                            Messenger.SendMessage m -> return ()
                                                    
                                                          
                     ) handleResults

                mapM (\(Messenger.RepeatCount x) -> 
                    modify (setCount x)) 
                    $ filter (\r -> case r of
                              Messenger.RepeatCount _ -> True
                              otherwise               -> False) handleResults
                return ()) (MessengerState 1 Nothing)
                
            return () 
--  forkIO $
--      Slack.withHandle slackConfig $ \h -> 
--          return () 
    return ()
--  runApp mainApp config
