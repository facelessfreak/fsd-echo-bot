{-# LANGUAGE OverloadedStrings #-}

module Logger 
    ( LogLevel (..)
    , LogMessage
    , Handle (..)
    , logPrefix
    ) where

import Data.Text (Text)
import Control.Concurrent ( Chan
                          , ThreadId)

data LogLevel 
    = Trace
    | Debug
    | Info
    | Warning
    | Error
    | Fatal
    deriving (Eq, Ord)

type OutputText = Text -> IO ()

data LogMessage
    = LogMessage 
        { logText  :: Text
        , logLevel :: LogLevel}

data Handle = Handle
    { logTrace      :: OutputText
    , logDebug      :: OutputText
    , logInfo       :: OutputText
    , logWarning    :: OutputText
    , logError      :: OutputText
    , logFatal      :: OutputText
    , threadId      :: IO ThreadId 
    }
    
logPrefix
    :: LogLevel
    -> Text
logPrefix level =
    case level of
    Trace      -> "TRACE"
    Debug      -> "DEBUG"
    Info       -> "INFO"
    Warning    -> "WARNING"
    Error      -> "ERROR"
    Fatal      -> "FATAL"


