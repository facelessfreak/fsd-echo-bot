{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text)
import           Data.List(sort)
import           Data.Maybe( fromMaybe)
import           Control.Monad.State
import           Network.HTTP.Req
import           Network.HTTP.Client(Proxy(Proxy))

type RequestExhauster = [RequestTimer] -> ((IMRequest, UTCTime), [RequestTimer])


addMillisec :: Integer -> UTCTime -> UTCTime
addMillisec sh = addUTCTime (fromRational (fromInteger sh / 1000))

generateQueue :: UTCTime -> [(IMRequest, Integer)] -> [RequestTimer]
generateQueue initTime reqRates = 
    sort $ concatMap (\(req, rate) -> 
        map (\x -> RequestTimer (addMillisec x initTime) req) [0,1..]) reqRates 


getNextIMRequest :: RequestExhauster
getNextIMRequest (t:ts) = ((request t, requestDate t), ts)

tick :: UTCTime -> IO ()
tick time = do
    currTime <- getCurrentTime
    if currTime >= time
        then return ()
        else tick time


go :: RequestExhauster -> [RequestTimer] -> IO()
go exhauster rt = do
    let ((request', requestUTC) , newRT) = exhauster rt
    let method' = getMethod (purpose request')
    let url'    = url request'
    let proxy'  = proxy request'
    let httpConfig' = defaultHttpConfig { httpConfigProxy = proxy'}
    tick requestUTC
    runReq httpConfig' $ do
      --let (url'', options) = fromJust $ parseUrlHttps ulr'
        return()
    return ()


main :: IO ()
main = do
    strJSON <- B.readFile "./config.json"
    let result     = decodeStrict strJSON :: Maybe ConfigJSON
    let config     = fromMaybe (error "Invalid token") result

    let tgUrl      = TG.getURLbyToken $ (t_token . ims_telegram . services) config
    let tgMethod   = Rget GET

    return ()


