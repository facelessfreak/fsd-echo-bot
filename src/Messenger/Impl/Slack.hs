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
    { getUpdates     = undefined
    , updatesChannel = undefined
    , send           = undefined
    , keyboard       = undefined
    }



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
