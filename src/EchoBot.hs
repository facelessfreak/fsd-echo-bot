module EchoBot 
    ( Handle (Handle)
    , sendMessage
    , getHelpInfo
    , getRepeatCount
    , setRepeatCount) where

import Data.Text (Text)
import Messenger ( Message
                 , Receiver)

data Handle =
    Handle { sendMessage
                 :: Receiver 
                 -> Text 
                 -> IO ()
           , getHelpInfo
                 :: Receiver 
                 -> IO () 
           , getRepeatCount
                 :: Receiver 
                 -> IO () 
           , setRepeatCount
                 :: Receiver 
                 -> Int 
                 -> IO ()
           }
