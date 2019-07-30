{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Messenger where

import Data.Text(Text)

data Message
data Receiver
data Update

data Handle
    = Handle
        { getUpdates
            :: IO [Update]
        , send
            :: Message
            -> Receiver
            -> IO ()
        }
