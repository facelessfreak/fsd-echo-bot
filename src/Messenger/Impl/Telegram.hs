module Messenger.Impl.Telegram where


getURLbyToken :: String -> String
getURLbyToken token = "https://api.telegram.org/bot" ++ token ++ "/getUpdates"
