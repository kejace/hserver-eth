module Handler.PushTransaction where

import Import
import Yesod.Core.Handler

getPushTransactionR :: Handler ()
getPushTransactionR = do
   addHeader "Access-Control-Allow-Origin" "*"   
   sendFile "text/js" "static/js/pushtx.js"


