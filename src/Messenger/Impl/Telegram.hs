module Messenger.Impl.Telegram 
    ( withHandle
    , Config) where

import Messenger

data Config = Config {}

new 
    :: Config 
    -> IO Handle
new = 
    undefined

close 
    :: Handle
    -> IO ()
close =
    undefined

withHandle
    :: Config
    -> (Handle -> IO a)
    -> IO a
withHandle =
    undefined


getURLbyToken :: String -> String
getURLbyToken token = "https://api.telegram.org/bot" ++ token ++ "/getUpdates"
