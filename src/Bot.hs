module Bot where
import qualified Messenger
import qualified Commander
import Data.Map as Map
import Control.Concurrent ( Chan
                          , newChan
                          , writeChan
                          , readChan)

type Receiver = Messenger.Receiver
type Command = Commander.Command
type CommandsMap    = [(Receiver, [Command])]
type CommandsChan   = Chan CommandsMap
type ChannelsMap    = Map.Map Receiver CommandsChan

data ChatState

runBot :: CommandsChan 
       -> IO ()
runBot commandsChan = do
    channelsMap_ref <- newIORef Map.empty
    forever $ do
        commandsMap <- readChan commandChan
        flip mapM_ commandsMap \(receiver, commands) -> do
            channelsMap <- readIORef channelsMap_ref
            channel <-  case Map.lookup receiver channelsMap of
                        Noting          -> do
                            newChannel <- newChan
                            modifyIORef channelsMap_ref $ Map.insert receiver newChannel channelsMap
                            pure newChannel
                        Just channel    -> pure channel







