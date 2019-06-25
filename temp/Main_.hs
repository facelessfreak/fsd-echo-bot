{-# LANGUAGE RankNTypes 
           , ScopedTypeVariables 
           , FlexibleInstances 
           , MultiParamTypeClasses 
           , AllowAmbiguousTypes
           , FlakffexibleContexts#-}

module Main_ where

import qualified Data.Time.Clock as T   ( UTCTime
                                        , NominalDiffTime
                                        , addUTCTime
                                        , getCurrentTime
                                        , diffUTCTime)
import           Types 
import qualified Telegram                      as TG
import qualified Slack                         as SL
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text)
import           Data.List(sort)
import           Data.Maybe( fromJust
                           , fromMaybe)
import           Network.HTTP.Simple                           
import           Network.HTTP.Client
import           Control.Concurrent( threadDelay
                                   , forkIO
                                   , ThreadId)
import           Control.Monad.Reader
import           Control.Monad.Identity(Identity)
    

getTelegramConfig :: AppConfig -> TelegramConfig
getTelegramConfig = undefined

getSlackConfig :: AppConfig -> SlackConfig
getSlackConfig = undefined

startTelegram :: TelegramConfig -> IO ()
startTelegram = runTelegram telegramApp

runTelegram :: TelegramApp a -> TelegramConfig -> IO a
runTelegram a = runReaderT (unTgApp a)



recursRequest :: 
    ( NestedRequests m req res log
    , Configuration c m ) 
    => PollingDelay
    -> req 
    -> m ()
recursRequest delay req = do
    config <- getConfig
    ( logRes, res ) <- sendRequest req
    outputLog logRes
    ( logReq, maybeNextReq ) <- getNextRequest res
    outputLog logReq
    sleep delay
    recursRequest delay $ fromMaybe req maybeNextReq 


telegramApp :: ( Configuration TelegramConfig m )
               => m ()
telegramApp = do
    config <- getConfig
    
    return()

startSlack :: SlackConfig -> IO ()
startSlack = undefined

mainApp :: ( Configuration AppConfig m ) 
           => m ()
mainApp = do
    config  <- getConfig
    let tgConfig    = getTelegramConfig config
    let slkConfig   = getSlackConfig config
    fork . startTelegram $ tgConfig
    fork . startSlack    $ slkConfig
    return ()

runApp :: App a -> AppConfig -> IO a
runApp a = runReaderT (unApp a) 

main :: IO ()
main = do
    config <- undefined
    runApp mainApp config
