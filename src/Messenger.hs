module Messenger where

data Handle = Handle
    { getUpdates :: IO()
    }

