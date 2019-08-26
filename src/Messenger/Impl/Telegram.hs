{-# LANGUAGE OverloadedStrings #-}

module Messenger.Impl.Telegram 
    ( withHandle
    , new
    , updatesURLFromToken 
    , Config (..) ) where

import Reexport
import Messenger
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Types.Header ( hContentType )
import Data.Aeson

import qualified Data.ByteString.Lazy
import qualified Updater
import qualified Updater.Impl.TelegramPolling as Polling
import qualified Parsing.TelegramBody as TelegramBody

type Token  = String
type ChatID = String

data Config
  = Config 
    { token       :: String 
    , proxyServer :: Maybe Proxy }

convertConfig
  :: Config
  -> Polling.Config
convertConfig config =
  Polling.Config { Polling.token       = token config
                 , Polling.proxyServer = proxyServer config }

new 
  :: Config
  -> Updater.Handle
  -> IO Handle
new config h = do
  let pollingConfig = convertConfig config
  pure Handle
    { getUpdates = Updater.getUpdates h
    , updatesChannel = Updater.updatesChannel h
    , send = \m r -> do 
        responseBS <- getSendMessageResponse config m Nothing r
        pure () 
    , keyboard = \kbdMode maybeM r -> do 
      let message =
            case maybeM of
              Nothing -> Message "Choose from a variants"
              Just m  -> m
      responseBS <- getSendMessageResponse config message (Just kbdMode) r
      putStrLn $ bUnpack $ getResponseBody responseBS
      pure ()
    }

getSendMessageResponse
  :: Config
  -> Message
  -> Maybe KeyboardMode
  -> Receiver
  -> IO (Response ByteString)
getSendMessageResponse config message mbKeyboard receiver = do
  initRequest <- parseRequest $ apiURL (token config) ++ "/sendMessage"
  let chatId =
        case (chat receiver) of
          Left  s -> ('@':s)
          Right i -> show i
  let text =
        case message of
          Message t -> t
  let body = createRequestBody chatId text mbKeyboard
  httpBS $
    initRequest { method = "POST" 
                , proxy  = proxyServer config 
                , requestBody = body 
                , requestHeaders = [( hContentType, "application/json")]}
  
             

close 
  :: Handle
  -> IO ()
close = const $ pure ()

withHandle
  :: Config
--TODO: Passing Updater.Handle 
  -> (Handle -> IO ())
  -> IO ()
withHandle config action = do
  let pollingConfig = convertConfig config
  Polling.withHandle pollingConfig $ \h ->
    bracket (new config h ) close action


apiURL
  :: Token
  -> String
apiURL token = "https://api.telegram.org/bot" ++ token

updatesURLFromToken :: String -> String
updatesURLFromToken token = "https://api.telegram.org/bot" ++ token ++ "/getUpdates"

sendMessageURLFromToken
  :: String
  -> String
  -> String
  -> String
sendMessageURLFromToken token text chatId = 
  "https://api.telegram.org/bot" ++ token 
  ++ "/sendMessage?chat_id=" ++ chatId ++ "&text=" ++ text

sendKeyboardURL
  :: String
  -> Keyboard
  -> Maybe Message
  -> String
  -> String
sendKeyboardURL token kb maybeM chatId = 
  let textPiece =
        case maybeM of
          Nothing -> ""
          Just (Message t) -> "&text=" ++ (tUnpack t)
  in "https://api.telegram.org/bot"
    ++ token ++ "/sendMessage?chat_id=" 
    ++ chatId ++ textPiece

createKeybuttons
  :: Keyboard
  -> [[TelegramBody.Keybutton]]
createKeybuttons kb =
  map (\k1 -> map ( \k2 -> TelegramBody.Keybutton k2
                  ) k1
      ) kb

createKeyboard
  :: [[TelegramBody.Keybutton]]
  -> TelegramBody.Keyboard
createKeyboard = TelegramBody.Keyboard

createRequestBody
  :: ChatID
  -> Text
  -> Maybe KeyboardMode
  -> RequestBody
createRequestBody chatId text mbKeyboard =
  let tBody =
        TelegramBody.SendMessage 
          { TelegramBody.text     = text
          , TelegramBody.chatId   = chatId
          , TelegramBody.keyboard = 
            case mbKeyboard of
              Nothing     -> Nothing
              Just kMode  -> 
                case kMode of
                  CreateKbd k ->
                    Just $
                      ( createKeyboard . createKeybuttons) k 
                  RemoveKbd  ->
                    Just TelegramBody.RemoveKeyboard }
  in RequestBodyLBS $ encode tBody
