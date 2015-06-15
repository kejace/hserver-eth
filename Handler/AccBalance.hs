{-# LANGUAGE OverloadedStrings #-}


module Handler.AccBalance where

import Import

import Data.Aeson
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import Handler.Common
import Blockchain.Data.DataDefs
import Data.List
       
import Handler.JsonJuggler

import qualified Prelude as P
       
getAccBalanceR :: Integer -> Handler Value
getAccBalanceR n      = do addHeader "Access-Control-Allow-Origin" "*"
                           acc <- runDB $ selectList [ AddressStateRefBalance ==. n ] [LimitTo (fromIntegral $ fetchLimit :: Int)] :: Handler [Entity AddressStateRef]   
                           returnJson $ P.map asrToAsrPrime' (P.map entityVal (acc :: [Entity AddressStateRef])) 



