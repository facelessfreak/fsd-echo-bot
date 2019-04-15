module Types where

import Data.Time.Clock

data Messenger = Telegram | Slack
data HTTPMethod = POST | GET

data JSONTelegram = JSONTelegram  { t_token :: String
                                  , t_rate :: Int
                                  , t_useProxy :: Bool
                                  , t_proxy :: Proxy}
                                  
data JSONSlack = JSONSlack  { s_token :: String
                            , s_rate :: Int}

data Proxy = ProxyURL String
            |ProxyPort Int

data IMRequest = IMRequest  { url       :: String
                            , method    :: HTTPMethod
                            , proxy     :: Maybe Proxy
                            , messenger :: Messenger}

data RequestTimer = RequestDate UTCTime
                   |Request IMRequest
