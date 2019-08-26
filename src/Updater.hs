module Updater ( Update   (..)
               , Receiver (..)
               , Message  (..)
               , Handle   (..) ) where

import Control.Concurrent ( Chan
                          , ThreadId )

import Data.Text ( Text )

data Message
    = Message Text

data Receiver
    = Receiver 
        { chat :: Either String Integer
        , user :: Either String Integer}
        deriving (Eq, Ord )

data Update
    = Update
        { receiver  :: Receiver
        , message   :: Message}


data Handle =
    Handle 
        { getUpdates        :: IO [Update] 
        , updatesChannel    :: IO (Chan Update) 
        , threadId          :: IO ThreadId }
