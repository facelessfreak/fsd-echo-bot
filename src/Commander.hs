module Commander ( Handle) where

import qualified EchoBot
import qualified Messenger_ as Messenger
import Data.Text(Text)

type Message = Messenger.Message
type Receiver = Messenger.Receiver

data Command
    = SendMessage       Receiver Text
    | GetHelpInfo       Receiver
    | GetRepeatCount    Receiver
    | SetRepeatCount    Receiver Int

data Handle =
    Handle { makeCommand    :: Message -> IO [Command]
           , runCommand     :: Command -> IO () }
