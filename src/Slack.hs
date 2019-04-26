module Slack where

getURLbyTokenAndChannel :: String -> String -> String
getURLbyTokenAndChannel t ch =
    "https://slack.com/api/im.history?token="
    ++ t ++ "&channel=" ++ ch
