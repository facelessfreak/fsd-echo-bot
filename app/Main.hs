module Main where

import           Lib
import           Data.Time.Clock( UTCTime
                                , NominalDiffTime
                                , addUTCTime)
import           CustomTypes 
import qualified Telegram                      as TG
import qualified Slack                         as SL

import qualified Data.ByteString               as B
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text)
import           Data.List(sort)
import           Data.Maybe(fromMaybe)
import           Control.Monad.State
import           Network.HTTP.Req

type RequestExhauster = [RequestTimer] -> (IMRequest, [RequestTimer])


addMillisec :: Integer -> UTCTime -> UTCTime
addMillisec sh = addUTCTime (fromRational (fromInteger sh / 1000))

generateQueue :: UTCTime -> [(IMRequest, Integer)] -> [RequestTimer]
generateQueue initTime reqRates = 
    sort $ concatMap (\(req, rate) -> 
        map (\x -> RequestTimer (addMillisec x initTime) req) [0,1..]) reqRates 


getNextIMRequest :: RequestExhauster
getNextIMRequest (t:ts) = (request t, ts)

--go :: RequestExhauster -> [RequestTimer]

main :: IO ()
main = do
    strJSON <- B.readFile "./config.json"
    let result     = decodeStrict strJSON :: Maybe ConfigJSON
    let config     = fromMaybe (error "Invalid token") result
    let tgUrl      = TG.getURLbyToken $ (t_token . ims_telegram . services) config
    --let tgMethod   = 
    return ()


