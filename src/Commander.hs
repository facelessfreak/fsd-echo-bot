module Commander ( Handle (..) 
                 , InputCommand (..) 
                 , OutputCommand (..) ) where

import qualified Messenger as Messenger
import Data.Text(Text)

type Message = Messenger.Message
type Receiver = Messenger.Receiver
type Count = Int

data InputCommand
    = EchoMessage       Receiver Text
    | GetHelpInfo       Receiver
    | GetRepeatCount    Receiver
    | SetRepeatCount    Receiver Int

data OutputCommand
    = SendMessage   Receiver Text Count
    | SendKeyboard  Receiver Text

data Handle =
    Handle { makeCommand    :: Message -> IO [InputCommand] }
