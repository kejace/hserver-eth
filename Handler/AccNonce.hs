module Handler.AccNonce where

import Import

import Database.Persist       
import Database.Persist.TH
import Database.Persist.Postgresql
import qualified Prelude as P
import Blockchain.Data.Address
import Numeric
import qualified Blockchain.Data.DataDefs as DD
       
import Handler.PQuery

import qualified Data.Text as T
       
-- Parses addresses from hex      
getAccNonceR :: Integer -> Handler Value
getAccNonceR nonce =   do
                           addHeader "Access-Control-Allow-Origin" "*"
                           addr <- runDB $ selectList [ AddressStateRefNonce ==. nonce ] [ LimitTo 100 ] :: Handler [Entity AddressStateRef]
                           returnJson $ P.map entityVal addr

