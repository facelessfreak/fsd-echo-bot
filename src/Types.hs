module Types where

data MessengerSettings = MessengerSettings
  { messenger :: Messenger 
  , token     :: String
  , proxy     :: Maybe Proxy }

data Proxy = Proxy
  { host :: String 
  , port :: Int }

data Messenger
  = Telegram
  | Slack
  | UnknownMessenger

