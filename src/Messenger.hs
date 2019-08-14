{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Messenger
    ( Handle   (..)
    , Message  (..)
    , Update   (..)
    , Receiver (..) ) where

import Data.Text(Text)
import Updater ( Message  (..)
               , Receiver (..)
               , Update   (..) )

data Handle
    = Handle
        { getUpdates
            :: IO [Update]
        , send
            :: Message
            -> Receiver
            -> IO ()
        }
