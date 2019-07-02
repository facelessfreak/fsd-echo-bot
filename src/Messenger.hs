{-# LANGUAGE MultiParamTypeClasses #-}

module Messenger 
    ( Handle
    , Response
    , Update 
    , MessageID (..)
    , getUpdates
    , sendMessage
    , muContent
    , Text ) where

import Data.Text (Text)

data MessageID
    = MessageID Integer
    | MessageTS Double
    
data ChatID
    = ChatIDNumber Integer
    | ChatIDString String

data Content
    = ContentText Text
    
data Receiver
    = ChatID Int
    | ChannelToken Int String

data Response

data Update 
    = Update 
        { muContent :: !(Content) 
        , muId      :: !(MessageID)
        , muChat    :: !(ChatID)
        }

data Handle 
    = Handle
        { getUpdates    :: Maybe MessageID
                           -> IO [Update]
        , sendMessage   :: !( Receiver 
                           -> Content 
                           -> IO Response)
        }
