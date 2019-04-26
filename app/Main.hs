{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Data.Time.Clock( UTCTime
                                , NominalDiffTime
                                , addUTCTime)
import           CustomTypes
import qualified Data.ByteString               as B
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text)
import           Data.List(sort)

addMillisec :: Integer -> UTCTime -> UTCTime
addMillisec sh = addUTCTime (fromRational (fromInteger sh / 1000))

generateQueue :: UTCTime -> [(IMRequest, Integer)] -> [RequestTimer]
generateQueue initTime reqRates = 
    sort $ concatMap (\(req, rate) -> 
        map (\x -> RequestTimer (addMillisec x initTime) req) [0,1..]) reqRates 

main :: IO ()
main = do
    strJSON <- B.readFile "./config.json"
    let result = decodeStrict strJSON :: Maybe ConfigJSON
    putStrLn $ case result of
        Nothing     -> error "Invalid JSON"
        Just config -> (t_token . ims_telegram . services) config
