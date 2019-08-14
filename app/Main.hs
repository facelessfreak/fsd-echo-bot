{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Reexport

import qualified Logger
import qualified Logger.Impl.Console as Console
import qualified Parsing.Config as Config

import Network.HTTP.Simple ( Proxy (..) )
import Data.Aeson ( decodeStrict )
import System.Exit
import System.Environment
import Messenger.Impl.Telegram as Telegram
import App ( runApp 
           , Env (..) )

logFatal
    :: Logger.Handle
    -> String
    -> IO ()
logFatal logger = do
    Logger.logFatal logger . tPack

logAndExit
    :: Logger.Handle
    -> String
    -> IO a
logAndExit logger message = do
    logFatal logger message
    exitFailure
    
main :: IO ()
main = 
    Console.withHandle $ \l -> do
    getEnvResult <- try $
        getEnv "HOME" :: IO (Either SomeException String)
    homePath <-
        case getEnvResult of
        Left e    -> logAndExit l "Can't find env 'HOME'" 
        Right env -> pure env
    let configPath = homePath ++ "/config.json"
    readConfigResult <- try $ 
        bReadFile configPath :: IO (Either SomeException ByteString)
    configBS <-
        case readConfigResult of
        Left e   -> logAndExit l ("Can't find config file at " ++ configPath)
        Right bs -> pure bs
    let maybeConfig = decodeStrict configBS :: Maybe [Config.Messenger] 
    parsedConfig <-
        case maybeConfig of
        Nothing  -> logAndExit l ("Can't parse config file at " ++ configPath)
        Just cfg -> pure cfg
    runApp $ Env { config = parsedConfig
                 , logger = l } 
