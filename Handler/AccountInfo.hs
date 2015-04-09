module Handler.AccountInfo where

import Import

getAccountInfoR :: Handler Html
getAccountInfoR = do addHeader "Access-Control-Allow-Origin" "*"
                     defaultLayout [whamlet| <h1> Accounts
                                                    

                                                <h2> Accounts are easily queried.

                                                <p>

                                                <ul>
                                                      <li> What's going on at address <a href="/query/account/address/4c712ac73a53046ce491d95f0aebf8ef8cb057">4c712ac73a53046ce491d95f0aebf8ef8cb057</a>? 

                                                      <li> Which accounts have nonce equal to <a href="/query/account/nonce/3">three</a>?
 
                                                      <li> Does anyone have balance  <a href="/query/account/balance/33000000000000000000">33000000000000000000</a>?
                                             |]

