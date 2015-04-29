{-# LANGUAGE OverloadedStrings #-}

module Handler.TxAddress where

import Import


import Handler.Common
import Blockchain.Data.DataDefs

import Database.Persist       
import Database.Persist.TH
import Database.Persist.Postgresql
import qualified Prelude as P
import Blockchain.Data.Address
<<<<<<< HEAD
--import Blockchain.Data.RawTransaction
=======
import Blockchain.Data.RawTransaction
>>>>>>> upstream/master
import Blockchain.ExtWord
import Numeric

import qualified Data.Text as T

import qualified Database.Esqueleto as E

-- Parses addresses from hex      
getTxAddressR :: Text -> Handler Value
getTxAddressR address = do
                           addHeader "Access-Control-Allow-Origin" "*"
                           addr <- runDB $ selectList ( [ (RawTransactionFromAddress ==. (Address wd160)) ]
                                                        ||. [ RawTransactionToAddress ==. (Just (Address wd160)) ] )
                                   [ LimitTo (fromIntegral $ fetchLimit :: Int), Desc RawTransactionNonce  ] :: Handler [Entity RawTransaction]
                           returnJson $ P.map entityVal addr
                         where
                           ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]
