{-# LANGUAGE OverloadedStrings #-}

module Handler.BlkCoinbase where
import Import


import Handler.Common

import Database.Persist       
import Database.Persist.TH
import Database.Persist.Postgresql
import qualified Prelude as P
import Blockchain.Data.Address
import Blockchain.ExtWord
import Numeric
import Blockchain.Data.DataDefs


import qualified Data.Text as T
       

-- Parses coinbase from hex      
getBlkCoinbaseR :: Text -> Handler Value
getBlkCoinbaseR address = do
                           addHeader "Access-Control-Allow-Origin" "*"
                           addr <- runDB $ selectList [ BlockDataRefCoinbase ==. (Address wd160) ] [ LimitTo (fromIntegral $ fetchLimit :: Int)  ] :: Handler [Entity BlockDataRef] 
                           returnJson $ P.map entityVal addr `debug` (show wd160)
                         where
                           ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]

                    
