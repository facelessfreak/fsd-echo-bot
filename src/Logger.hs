module Logger where

import Data.Text (Text)

data LogLevel 
    = Trace
    | Debug
    | Info
    | Warning
    | Error
    | Fatal
    deriving (Eq, Ord)

data Handle m = Handle
    { outputLog :: LogLevel -> Text -> m ()
    }
    

