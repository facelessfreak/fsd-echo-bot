{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           CustomTypes
import qualified Data.ByteString               as B
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text

main :: IO ()
main = do
    strJSON <- B.readFile "./config.json"
    let result = decodeStrict strJSON :: Maybe ConfigJSON
    putStrLn $ case result of
        Nothing     -> error "Invalid JSON"
        Just config -> (t_token . ims_telegram . services) config
