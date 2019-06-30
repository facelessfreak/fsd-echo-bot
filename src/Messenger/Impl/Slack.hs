module Messenger.Impl.Slack
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


getURLbyTokenAndChannel :: String -> String -> String
getURLbyTokenAndChannel t ch =
    "https://slack.com/api/im.history?token="
    ++ t ++ "&channel=" ++ ch
