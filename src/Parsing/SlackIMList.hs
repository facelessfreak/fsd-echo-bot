{-# LANGUAGE OverloadedStrings #-}

module Parsing.SlackIMList where

import Reexport
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable ( asum )

import Control.Applicative ( (<$>) 
                           , (<*>) )

data Results
    = IMs [IM]
    | Err String
instance FromJSON Results where
    parseJSON (Object v ) = 
        asum [ IMs <$> v .: "ims"
             , Err <$> v .: "error" ]

data IM =
    IM { idChannel :: String }

instance FromJSON IM where
    parseJSON (Object im) =
        IM <$> im .: "id"

data Response =
    Response { ok      :: Bool 
             , results :: Results }

instance FromJSON Response where
    parseJSON (Object resp) =
        Response
            <$> resp .: "ok"
            <*> asum [ resp .: "ims"
                     , resp .: "error" ]


