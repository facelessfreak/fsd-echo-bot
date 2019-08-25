{-# LANGUAGE OverloadedStrings #-}

module Messenger.Impl.Slack
    ( withHandle
    , Config) where

import           Reexport
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header ( hContentType )
import           Messenger
import qualified Data.ByteString.Lazy
import qualified Updater
import qualified Updater.Impl.SlackPolling as Polling


data Config = 
  Config { token       :: String
         , proxyServer :: Maybe Proxy }

type Token        = String
type URL          = String
type APIMethod    = String
type RawResponse  = Response ByteString


createPollingConfig
  :: Config
  -> Polling.Config
createPollingConfig config' =
  Polling.Config { Polling.token       = token config' 
                 , Polling.proxyServer = proxyServer config' }

new 
  :: Config 
  -> Updater.Handle
  -> IO Handle
new config' updater' = do
  let pollingConfig = createPollingConfig config'
  pure Handle
    { getUpdates     = Updater.getUpdates updater'
    , updatesChannel = Updater.updatesChannel updater'
    , send           = \ message' receiver' -> do 
      undefined
    , keyboard       = undefined
    }

sendMessage
  :: Config
  -> Message
  -> Receiver
  -> IO RawResponse
sendMessage config' message' receiver'  = do
  let channel'  = fromLeft "" $ chat receiver'
  when (channel' == "") $ do 
    -- TODO : logging
    pure ()
  let token'    = token config'
  let url'      = getAPIURL token' $ "chat.postMessage" 
  initRequest   <- parseRequest $ 
    url' <> "&channel=" <> channel'
  undefined

getAPIURL
  :: Token
  -> APIMethod
  -> URL
getAPIURL token' apiMethod' =
  "https://slack.com/api/" <> apiMethod'
  <> "?token=" <> token'



close 
  :: Handle
  -> IO ()
close _ = pure ()

withHandle
  :: Config
  -> (Handle -> IO ())
  -> IO ()
withHandle = undefined


getURLbyTokenAndChannel :: String -> String -> String
getURLbyTokenAndChannel t ch =
  "https://slack.com/api/im.history?token="
  ++ t ++ "&channel=" ++ ch
