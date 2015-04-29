module Handler.AccNonce where

import Import

import Database.Persist       
import Database.Persist.TH
import Database.Persist.Postgresql
import Blockchain.Data.DataDefs
import qualified Prelude as P
import qualified Database.Esqueleto as E

import Data.List
import Handler.Common

import qualified Data.Text as T
       
-- Parses addresses from hex      
getAccNonceR :: Integer -> Handler Value
getAccNonceR nonce =   do
                                   addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.select $
                                        E.from $ \(acc) -> do
                                        E.where_ ( (acc E.^. AddressStateRefNonce E.==. E.val nonce ) )
                                        return acc
                                   returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}


