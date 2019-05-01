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

type RequestExhauster = [RequestTimer] -> ((IMRequest, UTCTime), [RequestTimer])


addMillisec :: Integer -> UTCTime -> UTCTime
addMillisec sh = addUTCTime (fromRational (fromInteger sh / 1000))

generateQueue :: UTCTime -> [(IMRequest, Integer)] -> [RequestTimer]
generateQueue initTime reqRates = 
    sort $ concatMap 
    (\(req, rate) -> map (\x -> RequestTimer (addMillisec (x*rate) initTime) req) [0,1..]) reqRates 


getNextIMRequest :: RequestExhauster
getNextIMRequest (t:ts) = ((requestData t, requestTime t), ts)

await :: UTCTime -> IO ()
await time = do
    currTime <- getCurrentTime
    if currTime >= time
        then return ()
        else await time

go :: RequestExhauster -> [RequestTimer] -> IO()
go exhauster rt = do
    let ((requestData', requestTime') , requestTimers) = exhauster rt
    let method'     = method    requestData'
    let url'        = url       requestData'
    let proxy'      = proxy     requestData'

    await requestTime'


main :: IO ()
main = do
    strJSON <- B.readFile "./config.json"
    let result     = decodeStrict strJSON :: Maybe ConfigJSON
    let config     = fromMaybe (error "Invalid token") result

    let tgUrl      = TG.getURLbyToken $ (t_token . ims_telegram . services) config
    let tgMethod   = HttpGET

    return ()


