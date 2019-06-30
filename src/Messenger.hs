{-# LANGUAGE MultiParamTypeClasses #-}

module Messenger 
    ( Handle
    , MessengerResponse
    , MessengerUpdate 
    , MessageID (..)
    , MessageHandleResult (..)
    , getUpdates
    , sendMessage
    , handleContent
    , muContent
    , Text ) where

import Data.Text (Text)

data MessageID
    = MessageID Integer
    | MessageTS Double
    
data ChatID
    = ChatIDNumber Integer
    | ChatIDString String

data MessageCommand 
    = Help
    | Repeat
    | SetRepeatCount Int

data MessageHandleResult 
    = Empty
    | RepeatCount Int
    | SendMessage Text

data MessageContent
    = MessageText Text
    | MessageCommand MessageCommand
    
data MessageReceiver
    = ChatId Int
    | ChannelToken Int String


data MessengerResponse
data MessengerUpdate 
    = MessengerUpdate 
        { muContent :: !(MessageContent) 
        , muId      :: !(MessageID)
        , muChat    :: !(Integer)
        }

data Handle 
    = Handle
        { getUpdates    :: Maybe MessageID
                           -> IO [MessengerUpdate]
        , sendMessage   :: !( MessageReceiver 
                           -> Text 
                           -> IO MessengerResponse)
        , handleContent :: !( MessageContent 
                           -> IO MessageHandleResult)
        }
