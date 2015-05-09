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
import Blockchain.Data.RawTransaction
import Blockchain.ExtWord
import Numeric

import qualified Data.Text as T

import qualified Database.Esqueleto as E
import Handler.JsonJuggler

-- Parses addresses from hex      
getTxAddressR t = (getTxAddressR' t 0)

getTxAddressR' :: Text -> Integer -> Handler Value
getTxAddressR' address offset = do
                           addHeader "Access-Control-Allow-Origin" "*"
                           addr <- runDB $ selectList ( [ (RawTransactionFromAddress ==. (Address wd160)) ]
                                                        ||. [ RawTransactionToAddress ==. (Just (Address wd160)) ] )
                                   [ LimitTo limit, OffsetBy (limit * off) , Desc RawTransactionNonce  ] :: Handler [Entity RawTransaction]
                           returnJson $ P.map rtToRtPrime (P.map entityVal (addr :: [Entity RawTransaction]))
                         where
                           ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]
                           limit = (fromIntegral $ fetchLimit :: Int)
                           off = (fromIntegral $ offset :: Int)