{-# LANGUAGE OverloadedStrings #-}

module Messenger.Impl.Telegram 
    ( withHandle
    , new
    , updatesURLFromToken 
    , Config (..) ) where

import Reexport
import Messenger
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson

import qualified Updater
import qualified Updater.Impl.TelegramPolling as Polling

data Config
    = Config 
        { token       :: String 
        , proxyServer :: Maybe Proxy }

convertConfig
    :: Config
    -> Polling.Config
convertConfig config =
    Polling.Config { Polling.token       = token config
                   , Polling.proxyServer = proxyServer config }

new :: Config
    -> Updater.Handle
    -> IO Handle
new config h = do
    let pollingConfig = convertConfig config
    pure Handle
        { getUpdates = Updater.getUpdates h
        , send = \m r -> do 
            responseBS <- getSendMessageResponse config m r
            pure () }

getSendMessageResponse
    :: Config
    -> Message
    -> Receiver
    -> IO (Response ByteString)
getSendMessageResponse config message receiver = do
    initRequest <- parseRequest $
        sendMessageURLFromToken
            (token config) 
            (case message of
             Message t -> tUnpack t)
            (case (chat receiver) of
             Left s -> ('@':s)
             Right i -> show i)
    httpBS $ initRequest { method = "GET"
                         , proxy = proxyServer config }
               

close 
    :: Handle
    -> IO ()
close = undefined

withHandle
    :: Config
    -> (Handle -> IO ())
    -> IO ()
withHandle config action = do
    let pollingConfig = convertConfig config
    Polling.withHandle pollingConfig $ \h ->
        bracket (new config h ) close action


updatesURLFromToken :: String -> String
updatesURLFromToken token = "https://api.telegram.org/bot" ++ token ++ "/getUpdates"

sendMessageURLFromToken
    :: String
    -> String
    -> String
    -> String
sendMessageURLFromToken token text chatId = "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ chatId ++ "&text=" ++ text
