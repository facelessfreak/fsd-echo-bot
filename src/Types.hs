{-# LANGUAGE   OverloadedStrings
             , GeneralizedNewtypeDeriving
             , FunctionalDependencies
             , FlexibleInstances 
             , UndecidableInstances 
             , KindSignatures
             , MonoLocalBinds
             , RankNTypes #-}

module Types where

import             Data.Time.Clock(UTCTime)
import             Data.Aeson
import             Data.Text(Text)
import             Control.Applicative( (<$>) 
                                      , (<*>))
import             Control.Monad(mzero)
import             Network.HTTP.Client( Proxy(Proxy)
                                      , Response
                                      , method
                                      , proxy
                                      , parseRequest_)
import             Network.HTTP.Simple ( httpBS)
import             Control.Monad.Reader
import             Control.Monad.State
import             Control.Concurrent( threadDelay
                                       , forkIO
                                       , ThreadId)

import qualified   Data.ByteString.Char8 as BC

--
-- |Typeclasses|
--

class ( MonadReader a m 
      , Application m )
      => HasConfig m a where
    getConfig :: m a

class ( MonadIO m )  
      => HasIO m where
    fork  :: IO () -> m ThreadId
    sleep :: Int   -> m ()


class HTTPRequest a

class HTTPResponse a

class ( HTTPRequest req
      , HTTPResponse res)
      => HasHTTP req res

class ( HasHTTP req res 
      , WithLogging app log ) 
      => WorkWithRequest app req res log
      | app req -> res 
      , app res -> req
      , app req res -> log where
    sendRequest    :: req -> app ( log, res )
    handleResponse :: res -> app ( log, () )

class ( IsLog a 
      , HasIO m ) 
      => WithLogging (m :: * -> *) a where
    outputLog :: a -> m ()

class IsLog a

class ( WorkWithRequest app req res log )
      => NestedRequests app req res log
      | app req -> res
      , app res -> req 
      , app req res -> log where
    getNextRequest :: res -> app ( log, Maybe req )

class ( MonadState s m ) 
      => HasState m s where
    getState :: m s
    setState :: s -> m ()

class ( HasIO a )
      => Application (a :: * -> *)

class ( Application a
      , MonadReader c a ) 
      => Configuration c (a :: * -> *) | a -> c

-- 
--
--


-- 
-- |Instances|
--

instance IsLog Log

instance Application App
instance Application TelegramApp 
instance Application SlackApp

instance ( Application a ) 
    => NestedRequests a IMRequest ResponseBS Log where
    getNextRequest = undefined

instance Configuration AppConfig App
instance Configuration TelegramConfig TelegramApp
instance Configuration SlackConfig SlackApp

instance HTTPRequest IMRequest
instance HTTPResponse ResponseBS

instance ( HTTPRequest req
         , HTTPResponse res) => HasHTTP req res

instance ( Application a 
         , IsLog l )
         => WithLogging a l where
    outputLog = undefined

instance ( Application a ) 
         => WorkWithRequest a IMRequest ResponseBS Log where
    sendRequest req = undefined
--                let initReq   = parseRequest_ $ imUrl req
--                initReq'  = 
--                    let methodBS = BC.pack $ show $ imMethod req
--                        proxy'   = imProxy req
--                    in initReq { method = methodBS, proxy = proxy'}
--            in httpBS initReq'
    handleResponse = undefined

instance 
    ( Application a
    , MonadIO a )
    => HasIO a  where
        fork  = liftIO . forkIO
        sleep = liftIO . threadDelay . (* 10^3)
    
instance
    ( Configuration c a 
    , Application a
    , MonadReader c a )
    => HasConfig a c where
        getConfig = ask

instance Show HttpMethod where
    show HttpGET  = "GET"
    show HttpPOST = "POST"

--
--
--

--
-- |Types|
--


type RequestExhauster = 
    [RequestTimer] 
    -> ((IMRequest, UTCTime), [RequestTimer])
type PollingDelay = Int


data AppConfig = 
    AppConfig
        { appTelegramReq     :: IMRequest 
        , appTelegramUpdRate :: Int }

data TelegramConfig = 
    TelegramConfig 
        { tcDelay :: Int
        , tcToken :: String }

data SlackConfig  = 
    SlackConfig 
        { scDelay :: Int
        , scToken :: String }

type ResponseBS = Response BC.ByteString

newtype App a = 
    App { unApp :: ReaderT AppConfig IO a }
        deriving ( Functor
                 , Applicative
                 , Monad
                 , MonadIO
                 , MonadReader AppConfig)

newtype TelegramApp a = 
    TelegramApp { unTgApp :: ReaderT TelegramConfig IO a }
        deriving ( Functor
                 , Applicative
                 , Monad
                 , MonadIO
                 , MonadReader TelegramConfig)

newtype SlackApp a = 
    SlackApp { unSlkApp :: ReaderT SlackConfig IO a }
        deriving ( Functor
                 , Applicative
                 , Monad
                 , MonadIO
                 , MonadReader SlackConfig)


data HttpMethod
    = HttpGET
    | HttpPOST 
        deriving Eq

data LogLevel 
    = Trace
    | Debug
    | Info
    | Warning
    | Error
    | Fatal

data Log = 
    Log { logLevel :: LogLevel
        , logMsg   :: Text}


--
--Telegram response JSON types
--
data TelegramUser = 
    TelegramUser 
        { telegramUserId           :: Integer
        , telegramUserIsBot        :: Bool
        , telegramUserFirstName    :: String
        , telegramUserLastName     :: String
        , telegramUserUsername     :: String
        , telegramUserLanguageCode :: String}

instance FromJSON TelegramUser where
    parseJSON (Object tgUser) = 
        TelegramUser 
            <$> tgUser .: "id"
            <*> tgUser .: "is_bot"
            <*> tgUser .: "first_name"
            <*> tgUser .: "last_name"
            <*> tgUser .: "username"
            <*> tgUser .: "language_code"


data TelegramChat = 
    TelegramChat 
        { telegramChatId        :: Integer
        , telegramChatFirstName :: String
        , telegramChatLastName  :: String
        , telegramChatUsername  :: String
        , telegramChatType      :: String}

instance FromJSON TelegramChat where
    parseJSON (Object tgChat) = 
        TelegramChat 
            <$> tgChat .: "id"
            <*> tgChat .: "first_name"
            <*> tgChat .: "last_name"
            <*> tgChat .: "username"
            <*> tgChat .: "type"
            
data TelegramUpdateMessage = 
    TelegramUpdateMessage 
        { telegramUpdateMsgId   :: Int
        , telegramUpdateMsgFrom :: TelegramUser
        , telegramUpdateMsgChat :: TelegramChat
        , telegramUpdateMsgDate :: Integer
        , telegramUpdateMsgText :: String}

instance FromJSON TelegramUpdateMessage where
    parseJSON (Object tgUpdMsg) = 
        TelegramUpdateMessage 
            <$> tgUpdMsg .: "message_id"
            <*> tgUpdMsg .: "from"
            <*> tgUpdMsg .: "chat"
            <*> tgUpdMsg .: "date"
            <*> tgUpdMsg .: "text"

data TelegramUpdateResult = 
    TelegramUpdateResult 
        { telegramUpdateResultId  :: Integer
        , telegramUpdateResultMsg :: TelegramUpdateMessage}

instance FromJSON TelegramUpdateResult where
    parseJSON (Object tgUpdResult) = 
        TelegramUpdateResult 
            <$> tgUpdResult .: "update_id"
            <*> tgUpdResult .: "message"

data TelegramUpdateResponse = 
    TelegramUpdateResponse 
        { telegramUpdateOk     :: Bool
        , telegramUpdateResult :: [TelegramUpdateResult]}

instance FromJSON TelegramUpdateResponse where
    parseJSON (Object tgUpdResponse) = 
        TelegramUpdateResponse 
            <$> tgUpdResponse .: "ok"
            <*> tgUpdResponse .: "result"
--
--
--


--
--Config JSON types
--
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

instance FromJSON JSONProxy where
    parseJSON (Object proxy) = 
        JSONProxy 
            <$> proxy .: "ip"
            <*> proxy .: "port"

instance FromJSON JSONTelegram where
    parseJSON (Object tg) = 
        JSONTelegram 
            <$> tg .:  "token"
            <*> tg .:  "rate"
            <*> tg .:? "proxy"

instance FromJSON JSONSlack where
    parseJSON (Object sl) = 
        JSONSlack 
            <$> sl .: "token"
            <*> sl .: "rate"

instance FromJSON IMServices where
    parseJSON (Object ims) = 
        IMServices 
            <$> ims .: "telegram"
            <*> ims .: "slack"

instance FromJSON ConfigJSON where
    parseJSON (Object conf) = 
        ConfigJSON 
            <$> conf .: "services"


--
--
--



data RequestPurpose
    = TelegramUpdates
    | SlackUpdates
    | SlackChannels 
        deriving (Eq, Ord, Show)

data Messenger
    = Telegram
    | Slack  
        deriving (Eq, Show)


data IMRequest = 
    IMRequest
        { imUrl       :: String
        , imProxy     :: Maybe Proxy
        , imMethod    :: HttpMethod
        , imPurpose   :: RequestPurpose
        , imMessenger :: Messenger} 
            deriving (Eq, Show)

data RequestTimer = 
    RequestTimer 
        { requestTime :: UTCTime
        , requestData :: IMRequest} 
            deriving (Eq, Show)

instance Ord RequestTimer where
    compare x y = 
        compare (requestTime x) (requestTime y)
