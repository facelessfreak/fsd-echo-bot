{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Reexport

import qualified Logger
import qualified Logger.Impl.Console     as Console
import qualified Parsing.Config          as Config
import           Messenger.Impl.Telegram as Telegram
import           Network.HTTP.Simple        ( Proxy (..) )
import           Data.Aeson                 ( decodeStrict )
import           System.Exit
import           System.Environment
import           App                        ( runApp 
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

getToken
  :: Config.Messenger
  -> String
getToken = tUnpack . Config.token

getMaybeProxy
  :: Config.Messenger
  -> Maybe Proxy
getMaybeProxy m =
  case Config.proxy m of
    Nothing -> Nothing
    Just p ->
      Just Proxy { proxyHost = bPack $ tUnpack $ Config.host p
                 , proxyPort = Config.port p }

getTelegramConfig
  :: Config.Messenger
  -> Telegram.Config
getTelegramConfig m =
  Telegram.Config { Telegram.token = getToken m
                  , Telegram.proxyServer = getMaybeProxy m }

runAppWithEnv
  :: Config.Messenger
  -> Logger.Handle
  -> IO ()
runAppWithEnv m l = do
  case Config.name m of
    "telegram" -> Telegram.withHandle (getTelegramConfig m) $ \h -> 
      runApp $ Env { messenger = h
                   , logger    = l }    


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
  flip mapM_ parsedConfig $ \messenger ->
    if (Config.name messenger) == "slack"
    then pure ()
    else withAsync ( runAppWithEnv messenger l ) $ \a -> wait a
