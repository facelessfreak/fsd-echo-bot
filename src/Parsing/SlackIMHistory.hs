{-# LANGUAGE OverloadedStrings #-}

module Parsing.SlackIMHistory where

import Reexport
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable ( asum )

import Control.Applicative ( (<$>) 
                           , (<*>) )


data Results
    = Messages [Message]
    | Err String

instance FromJSON Results where
    parseJSON (Object v ) = 
        asum [ Messages <$> v .: "messages"
             , Err      <$> v .: "error" ]

data Message =
    Message { subtype :: Maybe String 
            , text    :: Maybe Text 
            , user    :: Maybe String
            , ts      :: Double 
            , msgType :: String }

instance FromJSON Message where
    parseJSON (Object m) =
        Message
            <$> m .:? "subtype"
            <*> m .:? "text"
            <*> m .:? "user"
            <*> m .:  "ts"
            <*> m .:  "type"

data Response =
    Response { ok       :: Bool 
             , messages :: Results }

instance FromJSON Response where
    parseJSON (Object resp) =
        Response
            <$> resp .: "ok"
            <*> asum [ resp .: "messages"
                     , resp .: "error" ]


