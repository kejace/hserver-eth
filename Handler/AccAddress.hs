{-# LANGUAGE OverloadedStrings #-}

module Handler.AccAddress where
import Import


import Handler.Common
import Blockchain.Data.DataDefs
import Blockchain.Data.Address

import Database.Persist       
import Database.Persist.TH
import Database.Persist.Postgresql
import qualified Prelude as P
import Blockchain.Data.Address
import Blockchain.ExtWord
import Numeric
       

import qualified Data.Text as T
import Handler.JsonJuggler

       
-- Parses addresses from hex      
getAccAddressR :: Text -> Handler Value
getAccAddressR address = do
                           addHeader "Access-Control-Allow-Origin" "*"
                           addr <- runDB $ selectList [ AddressStateRefAddress ==. (Address wd160) ] [ LimitTo (fromIntegral $ fetchLimit :: Int)  ] :: Handler [Entity AddressStateRef]
                           returnJson $ P.map asrToAsrPrime (P.map entityVal (addr :: [Entity AddressStateRef])) -- consider removing nub - it takes time n^{2}
                         where
                           ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]

                    
