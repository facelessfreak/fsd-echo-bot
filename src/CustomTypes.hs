{-# LANGUAGE OverloadedStrings #-}

module CustomTypes where

import Data.Time.Clock(UTCTime)
import Data.Aeson
import Control.Applicative((<$>), (<*>))
import Control.Monad(mzero)


newtype ConfigJSON = ConfigJSON
    { services :: IMServices}

data IMServices = IMServices
    { ims_telegram :: JSONTelegram
    , ims_slack    :: JSONSlack}

data JSONTelegram = JSONTelegram  
    { t_token       :: String
    , t_rate        :: Int
    , t_useProxy    :: Bool
    , t_proxy       :: Proxy}
                                  
data JSONSlack = JSONSlack
    { s_token   :: String
    , s_rate    :: Int}

data Proxy = Proxy
    { proxyIp   :: String
    , proxyPort :: Int} deriving (Eq, Show)

data Messenger = Telegram 
                |Slack  deriving (Eq, Show)

data HTTPMethod = POST 
                 |GET   deriving (Eq, Show)


instance FromJSON Proxy where
    parseJSON (Object proxy) = Proxy <$> proxy .: "ip"
                                     <*> proxy .: "port"

instance FromJSON JSONTelegram where
    parseJSON (Object tg) = JSONTelegram <$> tg .: "token"
                                         <*> tg .: "rate"
                                         <*> tg .: "useProxy"
                                         <*> tg .: "proxy"

instance FromJSON JSONSlack where
    parseJSON (Object sl) = JSONSlack <$> sl .: "token"
                                      <*> sl .: "rate"

instance FromJSON IMServices where
    parseJSON (Object ims) = IMServices <$> ims .: "telegram"
                                        <*> ims .: "slack"

instance FromJSON ConfigJSON where
    parseJSON (Object conf) = ConfigJSON <$> conf .: "services"
                  


data IMRequest = IMRequest
    { url       :: String
    , method    :: HTTPMethod
    , proxy     :: Maybe Proxy
    , messenger :: Messenger} deriving (Eq, Show)

data RequestTimer = RequestTimer { requestDate :: UTCTime
                                 , request     :: IMRequest} deriving (Eq, Show)

instance Ord RequestTimer where
    compare x y = compare (requestDate x) (requestDate y)
