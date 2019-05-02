{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Lib
import           Data.Time.Clock( UTCTime
                                , NominalDiffTime
                                , addUTCTime
                                , getCurrentTime)
import           CustomTypes 
import qualified Telegram                      as TG
import qualified Slack                         as SL

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text)
import           Data.List(sort)
import           Data.Maybe( fromJust
                           , fromMaybe)
import           Network.HTTP.Simple                           
import           Network.HTTP.Client

type RequestExhauster = [RequestTimer] -> ((IMRequest, UTCTime), [RequestTimer])


addMillisec :: Integer -> UTCTime -> UTCTime
addMillisec sh = addUTCTime (fromRational (fromInteger sh / 1000))

makeRequestTimers :: UTCTime -> [(IMRequest, Integer)] -> [RequestTimer]
makeRequestTimers initTime reqRates =
    map (\(imReq, offset) -> RequestTimer (addMillisec offset initTime) imReq) $ 
    makeQueue reqRates

makeQueue :: [(a, Integer)] -> [(a, Integer)]
makeQueue [] = []
makeQueue l = go l [0,1..]
    where go l (x:xs) = map     (\(a, num) -> (a, toInteger x)) 
                       (filter  (\(a, num) -> mod x num == 0) l )
                        ++ go l xs

getNextIMRequest :: RequestExhauster
getNextIMRequest (t:ts) = ((requestData t, requestTime t), ts)

wait :: UTCTime -> IO ()
wait time = do
    currTime <- getCurrentTime
    if currTime >= time
        then do
             return ()
        else do
             wait time

responseHandler :: BC.ByteString -> RequestPurpose -> IO ()
responseHandler resBS _ = putStrLn $ BC.unpack resBS

convertProxy :: Maybe JSONProxy -> Maybe Proxy
convertProxy (Just (JSONProxy jHost jPort)) = Just $ Proxy (BC.pack jHost) jPort
convertProxy _                              = Nothing

start :: RequestExhauster -> [RequestTimer] -> IO()
start exhauster rt = do
    let ((requestData', requestTime') , requestTimers) = exhauster rt
    let method'         = imMethod    requestData'
    let url'            = imUrl       requestData'
    let proxy'          = imProxy     requestData'
    let purpose'        = imPurpose   requestData'

    wait requestTime'

    initReq <- parseRequest url'
    let methodBS        = BC.pack $ show method'
    let req             = initReq { method = methodBS
                                  , proxy  = proxy'}
    responseBS <- httpBS req
    let bodyResponseBS  = getResponseBody responseBS
    responseHandler bodyResponseBS purpose'
    
    start exhauster requestTimers

main :: IO ()
main = do
    strJSON <- B.readFile "./config.json"
    let result     = decodeStrict strJSON :: Maybe ConfigJSON
    let config     = fromMaybe (error "Invalid token") result

    let tgConfig   = (ims_telegram . services) config
    let tgUrl      = TG.getURLbyToken $ t_token tgConfig 
    let tgMethod   = HttpGET
    let tgProxy    = t_proxy tgConfig
    let tgRate     = t_rate tgConfig

    let tgProxy'   = convertProxy tgProxy 

    let tgRequest  = IMRequest { imUrl          = tgUrl
                               , imProxy        = tgProxy'
                               , imMethod       = tgMethod
                               , imPurpose      = TelegramUpdates
                               , imMessenger    = Telegram}

    currentTime <- getCurrentTime
    let requestTimers = makeRequestTimers currentTime [(tgRequest, toInteger tgRate)] 

    start getNextIMRequest requestTimers

    return ()


