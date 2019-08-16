{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


module Messenger
    ( Handle   (..)
    , Message  (..)
    , Update   (..)
    , Receiver (..) 
    , Keyboard (..) ) where

import Reexport
import Updater ( Message  (..)
               , Receiver (..)
               , Update   (..) )

type Keyboard = [[String]]

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
            :: Keyboard
            -> Maybe Message
            -> Receiver
            -> IO ()
        }
