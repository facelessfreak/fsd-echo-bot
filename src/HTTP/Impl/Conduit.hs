{-# LANGUAGE OverloadedStrings #-}

module HTTP.Impl.Conduit where

import HTTP
import Network.HTTP.Simple

data Config

new :: Config
    -> IO Handle
new config = undefined
    

close :: Handle
    -> IO ()
close = undefined

withHandle
    :: Config
    -> (Handle -> IO ())
    -> IO ()
withHandle =
    undefined
