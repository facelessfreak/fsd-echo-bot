{-# LANGUAGE OverloadedStrings #-}

module HTTP where

data Request

data Response

data Method
    = POST
    | GET

data Handle 
    = Handle
        { sendRequest :: Request -> IO Response
        }
