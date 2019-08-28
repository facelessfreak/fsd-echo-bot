module Types where

data Messenger = Messenger
  { name  :: String 
  , token :: String
  , proxy :: Maybe Proxy }

data Proxy = Proxy
  { host :: String 
  , port :: Int }

