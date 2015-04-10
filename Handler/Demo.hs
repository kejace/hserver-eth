module Handler.Demo where

import Import
import Yesod.Core.Handler

getDemoR :: Handler Html
getDemoR = do sendFile "text/html" "static/showBlocks.html"
