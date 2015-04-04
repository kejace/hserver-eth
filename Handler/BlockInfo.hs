module Handler.BlockInfo where

import Import

getBlockInfoR :: Handler Html
getBlockInfoR = do addHeader "Access-Control-Allow-Origin" "*"
                   defaultLayout [whamlet| Blocks are fetchable by a variety of queries.! |]
