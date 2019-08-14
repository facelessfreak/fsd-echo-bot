{-# LANGUAGE OverloadedStrings #-}

module Updater.Impl.TelegramPolling ( new
                                    , close
                                    , withHandle
                                    , Config (..) ) where

import Reexport
import Updater
import qualified Parsing.TelegramResponse as TelegramResponse
import Network.HTTP.Simple
import Network.HTTP.Client 
import Data.Aeson ( decodeStrict )


data Config
    = Config
        { token       :: String
        , proxyServer :: Maybe Proxy }

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
        let results = getTelegramResultsFromResponse responseBS
        let updateIDs = map TelegramResponse.updateId results
        writeIORef updateOffset $ Just (maximum updateIDs)
        mapM_ (writeChan chan . createUpdate ) results
    pure (chan, threadId)

getUpdateResponse
    :: Config
    -> Maybe Integer
    -> IO (Response ByteString)
getUpdateResponse config mbOffset = do
    initRequest <- parseRequest $
        updatesURLFromToken (token config) mbOffset
    httpBS $ initRequest { method = "GET"
                         , proxy  = proxyServer config}

getTelegramResultsFromResponse
    :: Response ByteString
    -> [TelegramResponse.Result]
getTelegramResultsFromResponse responseBS =
    let maybeResponse = tryDecodeResponse responseBS
    in  case maybeResponse of
        Nothing -> []
        Just response -> TelegramResponse.result response

tryDecodeResponse
    :: Response ByteString
    -> Maybe TelegramResponse.Response
tryDecodeResponse responseBS =
    let responseBodyBS = getResponseBody responseBS
    in  decodeStrict responseBodyBS

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
            ( TelegramResponse.text
            . TelegramResponse.message ) result }
    

updatesURLFromToken
    :: String
    -> Maybe Integer
    -> String
updatesURLFromToken token mbOffset = 
    let offsetId = case mbOffset of
                   Nothing -> ""
                   Just o  -> "/offset=" ++ show o
    in  "https://api.telegram.org/bot" ++ token ++ "/getUpdates" ++ offsetId


