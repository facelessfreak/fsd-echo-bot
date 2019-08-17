
{-# LANGUAGE OverloadedStrings #-}

module Updater.Impl.SlackPolling ( new
                                 , close
                                 , withHandle
                                 , Config (..) ) where

import Reexport
import Updater
import Network.HTTP.Simple
import Network.HTTP.Client 
import Data.Aeson ( eitherDecodeStrict )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import qualified Parsing.SlackIMList    as IMList
import qualified Parsing.SlackIMHistory as IMHistory
import Control.Concurrent.Async ( forConcurrently )
import Data.Functor ( (<&>) )
import Text.Printf ( printf ) 


data Config = Config
        { token       :: Token 
        , proxyServer :: Maybe Proxy }

type ChannelID  = String
type Token      = String
type Timestamp  = Double



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

getAPIURL
    :: Token
    -> String -- method
    -> String -- url
getAPIURL token' method' =
    "https://slack.com/api/" 
    ++ method' 
    ++ "?token="
    ++ token' 

tryDecodeResponseIMList
    :: Response ByteString
    -> Either String IMList.Response
tryDecodeResponseIMList responseBS =
    eitherDecodeStrict $ 
        getResponseBody responseBS

getChannels
    :: Config
    -> Token
    -> IO [ChannelID]
getChannels config token' = do
    initRequest <- parseRequest $ 
        getAPIURL token' "im.list"
    responseBS <- httpBS $ 
        initRequest { method = "GET" 
                    , proxy  = proxyServer config }
    let eitherResponse = tryDecodeResponseIMList responseBS
    case eitherResponse of
        Left e -> do
            error e -- TODO:logging
        Right response ->
            case IMList.ok response of
                False -> do
                    -- TODO: logging
                    pure []
                True ->
                    case IMList.results response of
                        IMList.Err s -> do
                            -- TODO: logging
                            pure []
                        IMList.IMs ims' -> do
                            pure $ map (IMList.idChannel) ims'

showTimeStamp
    :: Timestamp
    -> String
showTimeStamp = printf "%.6f"


tryDecodeResponseIMHistory
    :: Response ByteString
    -> Either String IMHistory.Response
tryDecodeResponseIMHistory responseBS =
    eitherDecodeStrict $ 
        getResponseBody responseBS

getMessages
    :: Config
    -> Maybe Timestamp
    -> Token
    -> ChannelID
    -> IO [IMHistory.Message]
getMessages config mbTS token' channel' = do
    let tsPiece = 
            case mbTS of
                Nothing -> ""
                Just ts -> "&oldest=" ++ showTimeStamp ts
    initRequest <- parseRequest $
        getAPIURL token' "im.history" ++ tsPiece
    responseBS <- httpBS $ initRequest 
        { method = "GET" 
        , proxy  = proxyServer config }
    let eitherResponse = tryDecodeResponseIMHistory responseBS
    case eitherResponse of
        Left e -> do
            -- TODO: logging
            pure []
        Right r -> 
            case IMHistory.ok r of
                False -> do
                    pure [] -- TODO:logging
                True ->
                    case IMHistory.messages r of
                        IMHistory.Err e -> do
                            pure [] -- TODO: logging
                        IMHistory.Messages msgs -> do
                            pure $ filter (\m -> IMHistory.subtype m == Nothing) msgs

createUpdate
    :: ChannelID
    -> IMHistory.Message 
    -> Update
createUpdate channel' message' =
    Update
        { receiver = Receiver 
            { chat = Left channel' 
            , user = Left $ fromMaybe "" $ IMHistory.user message' }
        , message = Message $
            ( fromMaybe "empty"
            . IMHistory.text ) message' }


getUpdatesChannel
    :: Config
    -> IO ((Chan Update), ThreadId)
getUpdatesChannel config = do
    mbTimestampRef  <- newIORef (Nothing :: Maybe Timestamp)
    chan            <- newChan
    let token'      = token config
    threadId        <- forkIO $ forever $ do
        mbTimestamp             <- readIORef mbTimestampRef
        channelIDs              <- getChannels config token'
        listOfListOfMessages    <- forConcurrently channelIDs $ \ channelID' -> do 
                 messages'    <- getMessages config mbTimestamp token' channelID'
                 let updates' =  messages' <&> createUpdate channelID'
                 mapM_ (writeChan chan) updates'
                 pure messages'
        let listOfMessages = concat listOfListOfMessages
        let timestampList = map (IMHistory.ts) listOfMessages
        if (length timestampList) == 0
        then pure ()
        else do
            let newTimestamp     = maximum timestampList
            let currentTimestamp = fromMaybe 0.0 mbTimestamp
            if newTimestamp > currentTimestamp 
            then writeIORef mbTimestampRef $ Just newTimestamp
            else pure ()
    pure (chan, threadId)


