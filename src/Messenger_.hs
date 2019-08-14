{-# LANGUAGE MultiParamTypeClasses #-}

module Messenger_
    ( Handle
    , Message
    , UserID
    , Receiver 
    , SendResult
    , getUpdates
    , sendMessage ) where

import Data.Text (Text)

type IntOrString = Either Int String

type MessageID  = IntOrString 
type ChatID     = IntOrString
type UserID     = IntOrString

data Message
    = Message { mReceiver   :: Receiver
              , mMessage    :: MessageID
              , mText       :: Text
              }

data Receiver
    = Receiver { chat :: ChatID
               , user :: UserID
               } deriving (Eq, Ord)

data SendResult

data Handle = Handle
        { getUpdates    :: MessageID 
                        -> IO [Message] 
        , sendMessage   :: Receiver 
                        -> Text 
                        -> IO SendResult
        }
