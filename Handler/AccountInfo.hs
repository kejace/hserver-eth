module Handler.AccountInfo where

import Import

getAccountInfoR :: Handler Html
getAccountInfoR = do addHeader "Access-Control-Allow-Origin" "*"
                     defaultLayout [whamlet| One can query accounts in several ways! |]
