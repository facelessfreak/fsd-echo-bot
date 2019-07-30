module Commander.Impl.Telegram where

import qualified Commander
import qualified EchoBot
import qualified Messenger_ as Messenger

type Message = Messenger.Message

new :: EchoBot.Handle 
    -> IO Commander.Handle
new h = do
    return Commander.Handle
        { Commander.makeCommand = \msg ->
            case msg of
            "/help" -> 
            
        , Commander.runCommand = \cmd ->
            case cmd of
            SendMessage r t  -> EchoBot.sendMessage h r t
            GetHelpInfo r    -> EchoBot.getHelpInfo h r
            GetRepeatCount r -> EchoBot.getRepeatCount h r
            SetRepeatCount c -> EchoBot.setRepeatCount h c
        }
    
