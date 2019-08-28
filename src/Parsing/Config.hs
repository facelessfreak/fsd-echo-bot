{-# LANGUAGE OverloadedStrings #-}


module Parsing.Config where

import Reexport
import Data.Aeson
import qualified Types

data Configuration = 
  Configuration
    { messengers :: [Types.Messenger] }

instance FromJSON Types.Messenger where
    parseJSON (Object m) =
        Messenger <$> m .:  "name"
                  <*> m .:  "token"
                  <*> m .:? "proxy"

instance FromJSON Types.Proxy where
    parseJSON (Object p) =
        Proxy <$> p .: "host"
              <*> p .: "port"


