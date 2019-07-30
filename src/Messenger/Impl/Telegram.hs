module Messenger.Impl.Telegram 
    ( withHandle
    , new
    , Config) where

import Messenger

data Config

new :: Config 
    -> IO Handle
new = do
    undefined

close 
    :: Handle
    -> IO ()
close =
    undefined

withHandle
    :: Config
    -> (Handle -> IO ())
    -> IO ()
withHandle =
    undefined

getURLbyToken :: String -> String
getURLbyToken token = "https://api.telegram.org/bot" ++ token ++ "/getUpdates"
