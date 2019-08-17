{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


module Messenger
    ( Handle       (..)
    , Message      (..)
    , Update       (..)
    , Receiver     (..) 
    , Keyboard     (..) 
    , KeyboardMode (..) ) where

import Reexport
import Updater ( Message  (..)
               , Receiver (..)
               , Update   (..) )

type Keyboard = [[String]]
data KeyboardMode 
    = CreateKbd Keyboard
    | RemoveKbd

data Handle
    = Handle
        { getUpdates
            :: IO [Update]
        , updatesChannel
            :: IO (Chan Update)
        , send
            :: Message
            -> Receiver
            -> IO ()
        , keyboard
            :: KeyboardMode
            -> Maybe Message
            -> Receiver
            -> IO ()
        }
