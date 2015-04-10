module Handler.Query where

import Import
import qualified QuerySchema as QS
import Data.Aeson
import Blockchain.Data.DataDefs
import Data.ByteString.Lazy as BS



getQueryR :: Handler Html
getQueryR = do
              addHeader "Access-Control-Allow-Origin" "*"
              defaultLayout $  [whamlet|<h1> Queries!

                                        <h3>
                                        <p>

                                        Follow the links to learn how to query <a href="/query/block">block</a> and <a href="/query/account">account</a> states. Resources
                                        are located via GET routes.
                                        They are meant to be invoked via XMLHttpRequest and return raw JSON. For something a bit prettier, look <a href="/demo">here</a>.
                               |]
                
