{-# LANGUAGE OverloadedStrings #-}

module Updater.Impl.TelegramPolling ( new
                                    , close
                                    , withHandle
                                    , Config (..) ) where

import Reexport
import Updater
import qualified Parsing.TelegramResponse as TelegramResponse
import qualified Logger as Logger 
import qualified Logger.Impl.Console as Console
import qualified Types
import Network.HTTP.Simple
import Network.HTTP.Client 
import Data.Aeson ( eitherDecodeStrict )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )


data Config
    = Config
        { token       :: String
        , proxyServer :: Maybe Types.Proxy }

new :: Config
    -> IO Handle
new config = do
    (chan, threadId) <- getUpdatesChannel config
    pure Handle
        { getUpdates     = getChanContents chan
        , updatesChannel = pure chan
        , threadId       = pure threadId }

close
    :: Handle
    -> IO ()
close h = threadId h >>= killThread

withHandle
    :: Config
    -> (Handle -> IO ())
    -> IO ()
withHandle config action = 
    bracket (new config) close action

getUpdatesChannel
    :: Config
    -> IO ((Chan Update), ThreadId)
getUpdatesChannel config = do
    updateOffset <- newIORef (Nothing :: Maybe Integer)
    chan         <- newChan
    threadId     <- forkIO $ forever $ do
        updateOffset_ <- readIORef updateOffset
        responseBS    <- getUpdateResponse config updateOffset_
        results <- getTelegramResultsFromResponse responseBS
        let updateIDs = map TelegramResponse.updateId results
        if (length updateIDs) == 0
        then pure ()
        else do
            writeIORef updateOffset $ Just $ (maximum updateIDs) + 1
            mapM_ (writeChan chan . createUpdate ) results
    pure (chan, threadId)

getUpdateResponse
    :: Config
    -> Maybe Integer
    -> IO (Response ByteString)
getUpdateResponse config mbOffset = do
    let url = updatesURLFromToken (token config) mbOffset
    initRequest <- parseRequest url
    httpBS $ initRequest { method = "GET"
                         , proxy  = proxyServer config}

getTelegramResultsFromResponse
    :: Response ByteString
    -> IO [TelegramResponse.Result]
getTelegramResultsFromResponse responseBS =
    let eitherResponse = tryDecodeResponse responseBS
    in  case eitherResponse of
            Left e -> do
                putStrLn "Can't decode Telegram response"
                putStrLn e
                error "error"
            Right response -> pure $ TelegramResponse.result response

tryDecodeResponse
    :: Response ByteString
    -> Either String TelegramResponse.Response
tryDecodeResponse responseBS =
    let responseBodyBS = getResponseBody responseBS
    in  eitherDecodeStrict responseBodyBS

createUpdate
    :: TelegramResponse.Result
    -> Update
createUpdate result =
    Update
        { receiver = Receiver
            { chat = Right $
                ( TelegramResponse.chatId
                . TelegramResponse.chat
                . TelegramResponse.message) result
            , user = Left $
                ( tUnpack
                . TelegramResponse.chatUsername 
                . TelegramResponse.chat 
                . TelegramResponse.message ) result }
        , message = Message $
            ( fromMaybe "empty"
            . TelegramResponse.text
            . TelegramResponse.message ) result }
    

updatesURLFromToken
    :: String
    -> Maybe Integer
    -> String
updatesURLFromToken token mbOffset = 
    let offsetId = case mbOffset of
                   Nothing -> ""
                   Just o  -> "?offset=" ++ show o
    in  "https://api.telegram.org/bot" ++ token ++ "/getUpdates" ++ offsetId
