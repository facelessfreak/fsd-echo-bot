{-# LANGUAGE OverloadedStrings #-}

module CustomTypes where

import Data.Time.Clock(UTCTime)
import Data.Aeson
import Control.Applicative((<$>), (<*>))
import Control.Monad(mzero)
import Network.HTTP.Client(Proxy(Proxy))

data HttpMethod = HttpGET | HttpPOST deriving Eq

instance Show HttpMethod where
    show HttpGET  = "GET"
    show HttpPOST = "POST"

newtype ConfigJSON = ConfigJSON
    { services :: IMServices}

data IMServices = IMServices
    { ims_telegram :: JSONTelegram
    , ims_slack    :: JSONSlack}

data JSONTelegram = JSONTelegram
    { t_token       :: String
    , t_rate        :: Int
    , t_proxy       :: Maybe JSONProxy}

data JSONSlack = JSONSlack
    { s_token   :: String
    , s_rate    :: Int}

data JSONProxy = JSONProxy
    { jProxyIp   :: String
    , jProxyPort :: Int} deriving (Eq, Show)

data RequestPurpose = TelegramUpdates
                     |SlackUpdates
                     |SlackChannels deriving (Eq, Ord, Show)

data Messenger = Telegram
                |Slack  deriving (Eq, Show)

instance FromJSON JSONProxy where
    parseJSON (Object proxy) = JSONProxy <$> proxy .: "ip"
                                         <*> proxy .: "port"

instance FromJSON JSONTelegram where
    parseJSON (Object tg) = JSONTelegram <$> tg .:  "token"
                                         <*> tg .:  "rate"
                                         <*> tg .:? "proxy"

instance FromJSON JSONSlack where
    parseJSON (Object sl) = JSONSlack <$> sl .: "token"
                                      <*> sl .: "rate"

instance FromJSON IMServices where
    parseJSON (Object ims) = IMServices <$> ims .: "telegram"
                                        <*> ims .: "slack"

instance FromJSON ConfigJSON where
    parseJSON (Object conf) = ConfigJSON <$> conf .: "services"



data IMRequest = IMRequest
    { imUrl       :: String
    , imProxy     :: Maybe Proxy
    , imMethod    :: HttpMethod
    , imPurpose   :: RequestPurpose
    , imMessenger :: Messenger} deriving (Eq, Show)

data RequestTimer = RequestTimer { requestTime :: UTCTime
                                 , requestData :: IMRequest} deriving (Eq, Show)

instance Ord RequestTimer where
    compare x y = compare (requestTime x) (requestTime y)
