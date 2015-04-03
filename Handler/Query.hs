module Handler.Query where

import Import
import qualified QuerySchema as QS
import Data.Aeson
import Blockchain.Data.DataDefs
import Data.ByteString.Lazy as BS



getQueryR :: Handler Html
getQueryR = do
              defaultLayout $  [whamlet|<h1> Queries!
                                        <h2>
                                             Right now, we have GET routes for querying \<a href="/query/block">block</a> and <a href="/query/account">account</a> states.
                               |]
                
