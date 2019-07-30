{-# LANGUAGE OverloadedStrings #-}

module EchoBot.Impl.IM where

import Data.Text ( Text
                 , unpack
                 , pack)
import qualified EchoBot as EchoBot
import qualified Messenger as Messenger
import Data.IORef
import Control.Exception ( bracket)
import qualified Data.Map as Map

data Config = 
    Config { initCount  :: Int
           , helpInfo   :: Text
           , repeatInfo :: Text}

type CountTable = Map.Map (Messenger.Receiver) Int
type Receiver = Messenger.Receiver

getCount :: CountTable -> Receiver -> Int -> Int
getCount m r i = case Map.lookup r m of
    Nothing -> i
    Just x  -> x

new :: Config 
    -> Messenger.Handle
    -> IO EchoBot.Handle
new config mHandle = do
    countMapRef <- newIORef $ Map.empty
    let sendMessage = Messenger.sendMessage mHandle
    return EchoBot.Handle 
        { EchoBot.sendMessage = \r t -> do
            countMap <- readIORef countMapRef
            let count = getCount countMap r $ initCount config
            sequence $ take count $ repeat $ sendMessage r t
            pure ()
        , EchoBot.getHelpInfo = \r -> do
            sendMessage r $ helpInfo config
            pure ()
        , EchoBot.getRepeatCount = \r -> do
            countMap <- readIORef countMapRef
            let count = getCount countMap r $ initCount config
            sendMessage r $ pack $ replace (unpack $ repeatInfo config) '_' $ show count
            pure ()
        , EchoBot.setRepeatCount = \r c -> do
            countMap <- readIORef countMapRef
            writeIORef countMapRef $ Map.insert r c countMap
            pure ()
        }

close 
    :: EchoBot.Handle
    -> IO ()
close h = return ()

replace :: String -> Char -> String -> String
replace s1 c s2 = 
    concat 
    . map (\[x] -> if x == c then s2 else [x]) 
    . map (\x -> [x]) 
        $ s1

withHandle
    :: Config
    -> Messenger.Handle
    -> (EchoBot.Handle -> IO ())
    -> IO ()
withHandle c mh f =
    bracket (new c mh ) close f




