{-# LANGUAGE OverloadedStrings #-}


module Parsing.Config where

import Reexport
import Data.Aeson

data Messenger = Messenger
    { name  :: Text 
    , token :: Text
    , proxy :: Maybe Proxy }

instance FromJSON Messenger where
    parseJSON (Object m) =
        Messenger <$> m .: "name"
                  <*> m .: "token"
                  <*> m .:? "proxy"

data Proxy = Proxy
    { host :: Text
    , port :: Int }

instance FromJSON Proxy where
    parseJSON (Object p) =
        Proxy <$> p .: "host"
              <*> p .: "port"


