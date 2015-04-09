module Handler.BlockInfo where

import Import

getBlockInfoR :: Handler Html
getBlockInfoR = do addHeader "Access-Control-Allow-Origin" "*"
                   defaultLayout [whamlet| <h1> Blocks
                                                    

                                                <h2> Blocks are fetchable by a variety of queries! 

                                                <p>

                                                <ul>
                                                      <li> Grab the genesis block by <a href="/query/block/number/0">number</a> 
                                                         or by <a href="/query/block/hash/fd4af92a79c7fc2fd8bf0d342f2e832e1d4f485c85b9152d2039e03bc604fdca">hash</a>.  
 
                                                      <li> Find blocks with a fair amount of <a href="/query/block/gasrange/lower/100000/upper/300000">gas usage</a>.
                                             |]
