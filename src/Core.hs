module Core where

import qualified Types
import qualified Network.HTTP.Client as HTTP
import Reexport

convertProxy
  :: Maybe Types.Proxy
  -> Maybe HTTP.Proxy
convertProxy maybeProxy =
  case maybeProxy of
    Nothing    -> Nothing
    Just proxy -> Just $
      HTTP.Proxy  { HTTP.proxyHost = bPack $ Types.host proxy
                  , HTTP.proxyPort = Types.port proxy }
