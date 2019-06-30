module HTTP where

data Request
data Response

data Handle m = Handle
    { sendRequest :: Request -> m Response
    }
