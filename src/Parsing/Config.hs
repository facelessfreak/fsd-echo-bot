{-# LANGUAGE OverloadedStrings #-}

module Parsing.Config where

import Reexport
import Data.Aeson
import qualified Types

data Configuration = 
  Configuration
    { messengers :: [Types.MessengerSettings] }

instance FromJSON Types.MessengerSettings where
    parseJSON (Object m) =
        Types.MessengerSettings
        <$> m .:  "name"
        <*> m .:  "token"
        <*> m .:? "proxy"

instance FromJSON Types.Proxy where
    parseJSON (Object p) =
        Types.Proxy
        <$> p .: "host"
        <*> p .: "port"

instance FromJSON Types.Messenger where
  parseJSON (String m) = pure $
    case m of
      "telegram" -> Types.Telegram
      "slack"    -> Types.Slack
      otherwise  -> Types.UnknownMessenger


