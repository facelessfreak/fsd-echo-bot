{-# LANGUAGE OverloadedStrings #-}

module Parsing.TelegramBody where

import Reexport
import Data.Aeson

data Keybutton = Keybutton
    { keyText :: String }


instance ToJSON Keybutton where
    toJSON (Keybutton keyT) = 
        object ["text" .= keyT ]
    toEncoding (Keybutton keyT ) =
        pairs ( "text" .= keyT )

data Keyboard = Keyboard [[Keybutton]]

instance ToJSON Keyboard where
    toJSON (Keyboard kb ) =
        object ["inline_keyboard" .= kb ]
    toEncoding (Keyboard kb ) =
        pairs ( "inline_keyboard" .= kb )

data SendMessage = SendMessage
    { text     :: Text
    , chatId   :: String
    , keyboard :: Maybe Keyboard }

instance ToJSON SendMessage where
    toJSON (SendMessage t c k) =
        object $ 
            [ "text" .= t
            , "chat_id" .= c ]
            ++ case k of
                 Nothing -> []
                 Just keys -> [ "reply_markup" .= keys ]
    toEncoding (SendMessage t c k) =
        pairs (  "text"         .= t 
              <> "chat_id"      .= c
              <> case k of
                    Nothing   -> mempty
                    Just keys -> "reply_markup" .= keys )
