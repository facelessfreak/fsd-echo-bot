# Echo-bot

For Telegram, the "getUpdates" method is used, the description of which is located is here https://core.telegram.org/bots/api#getting-updates

Sample URL to get the list of messages:

https://api.telegram.org/bot999999999:XXXXXXXXXXXXX/getUpdates

Where:
999999999:XXXXXXXXXXXXX - token to access the HTTP API


For slack, the "im.history" method is used, the description of which is located here https://api.slack.com/methods/im.history.

Sample URL to get the list of messages:

https://slack.com/api/im.history?token=xxxx-xxxxxxxxxx-xxxx&channel=D123456

Where:
xxxx-xxxxxxxxxx-xxxx - bot user OAuth access token
D123456 - channel identifier
