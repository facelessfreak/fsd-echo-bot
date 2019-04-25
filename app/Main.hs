{-# LANGUAGE OverloadedStrings #-}

module Main where

import              Lib
import              CustomTypes
import qualified    Data.ByteString as B
import              Data.Aeson
import              Data.Aeson.Types
import              Data.Text

{--extractObjectByPath :: Object -> [Text] -> Either String Object
extractObjectByPath obj path = case path of
                               []     -> Left "!@" 
                               (p:ps) -> parseEither (\obj -> go (obj .: p) ps) obj
                               where go m l = case l of
                                              []     ->     m               
                                              (k:ks) -> go (m >>= (.:k)) ks 

getStringFromObject :: Object -> Text -> Maybe String
getStringFromObject obj k = flip parseMaybe obj $ \obj' -> do
                                                           value <- obj' .: k
                                                           return value

data JSONType = Object' | String' | Number' | Bool'
data JSONPath = JSONPath [Text] JSONType
                                                
getStringFromJSONPath :: Object -> JSONPath -> Maybe String
getStringFromJSONPath obj jp = case jp of
                               JSONPath [] _           -> Nothing
                               JSONPath [x] String'    -> getStringFromObject obj x
                               JSONPath (x:xs) String' -> case extractObjectByPath obj [x] of
                                                          Left _     -> Nothing
                                                          Right obj' -> getStringFromJSONPath obj' (JSONPath xs String')
--}



-- getTokenPath = JSONPath ["services", "telegram", "proxy", "ip"] String' 


main :: IO ()
main = do
    strJSON <- B.readFile "./config.json"
    let result = decodeStrict strJSON :: Maybe ConfigJSON 
    putStrLn $ case result of
               Nothing      -> error "Invalid JSON"
               Just config  -> (t_token . ims_telegram . services) config
{--               Just conf    -> case getStringFromJSONPath conf getTokenPath of
                               Nothing  -> error "!!!" 
                               Just x   -> x --}


