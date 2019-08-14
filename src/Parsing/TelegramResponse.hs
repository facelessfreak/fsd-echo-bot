{-# LANGUAGE OverloadedStrings #-}

module Parsing.TelegramResponse ( Response (..) 
                                , Result (..)
                                , Message (..)
                                , Chat (..)
                                , From (..) ) where

import Reexport

import Data.Aeson hiding (Result)
import Data.Aeson.Types hiding (Result)
import Control.Applicative ( (<$>)
                           , (<*>) )


data Response = Response
    { isOk   :: Bool
    , result :: [Result] }

instance FromJSON Response where
    parseJSON (Object r) = 
        Response
            <$> r .: "ok"
            <*> r .: "result"

data Result = Result
    { message  :: Message
    , updateId :: Integer }

instance FromJSON Result where
    parseJSON (Object r) =
        Result
            <$> r .: "message"
            <*> r .: "update_id"

data Message = Message
    { chat      :: Chat
    , date      :: Integer
    , from      :: From
    , messageId :: Integer
    , text      :: Text }

instance FromJSON Message where
    parseJSON (Object m) =
        Message
            <$> m .: "chat"
            <*> m .: "date"
            <*> m .: "from"
            <*> m .: "message_id"
            <*> m .: "text"

data Chat = Chat
    { chatFirstName :: Text
    , chatId        :: Integer
    , chatLastName  :: Text
    , chatType      :: Text
    , chatUsername  :: Text }

instance FromJSON Chat where
    parseJSON (Object c) =
        Chat
            <$> c .: "first_name"
            <*> c .: "id"
            <*> c .: "last_name"
            <*> c .: "type"
            <*> c .: "username"

data From = From
    { fromFirstName     :: Text
    , fromId            :: Integer
    , isBot             :: Bool
    , fromLanguageCode  :: Text
    , fromLastName      :: Text
    , fromUserName      :: Text }

instance FromJSON From where
    parseJSON (Object f) = 
        From
            <$> f .: "first_name"
            <*> f .: "id"
            <*> f .: "is_bot"
            <*> f .: "language_code"
            <*> f .: "last_name"
            <*> f .: "username"
