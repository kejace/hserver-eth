module Handler.PushTransaction where

import Import
import Yesod.Core.Handler

getPushTransactionR :: Handler ()
getPushTransactionR = sendFile "text/js" "static/js/pushtx.js"


