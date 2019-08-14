{-# LANGUAGE OverloadedStrings #-}

module Logger.Impl.Console 
    ( new
    , close
    , withHandle
    ) where

import Control.Concurrent ( Chan
                          , newChan
                          , readChan
                          , writeChan
                          , killThread
                          , forkIO)
import Logger
import Data.Text ( Text
                 , append
                 , pack
                 , unpack)
import Data.Time.Clock ( UTCTime 
                       , getCurrentTime )
import Control.Exception ( bracket)
import Control.Monad ( forever)

type MessageChannel = Chan Text

new :: IO Handle
new = do
    logChan <- newChan
    threadId_ <- forkIO $ toLog logChan
    pure Handle 
        { logTrace      = logToChannel logChan Trace
        , logDebug      = logToChannel logChan Debug
        , logInfo       = logToChannel logChan Info
        , logWarning    = logToChannel logChan Warning
        , logError      = logToChannel logChan Error
        , logFatal      = logToChannel logChan Fatal
        , threadId      = pure threadId_ }

close
    :: Handle
    -> IO ()
close h = threadId h >>= killThread


withHandle
    :: (Handle -> IO ())
    -> IO ()
withHandle action =
    bracket new close action

toLog
    :: MessageChannel
    -> IO ()
toLog chan = do
    forever $ do
        msg <- readChan chan
        (putStrLn . unpack) msg


logToChannel
    :: MessageChannel 
    -> LogLevel
    -> Text 
    -> IO ()
logToChannel chan level text = do
    currentT <- getCurrentTime
    writeChan chan $ pack $
        (unpack $ Logger.logPrefix level) 
        ++ ": " ++ (unpack text) 
        ++ "["  ++ (show currentT) ++ "]"

