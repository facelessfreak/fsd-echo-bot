module Updater where

data Update

data Handle =
    Handle 
        {
            getUpdates :: IO [Update]
        }
