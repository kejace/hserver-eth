{-# LANGUAGE OverloadedStrings #-}


module Handler.AccBalance where

import Import

import Data.Aeson
import qualified Blockchain.Data.DataDefs as DD
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import Handler.PQuery
import Data.List
       
import qualified Prelude as P
       
getAccBalanceR :: Integer -> Handler Value
getAccBalanceR n      = do addHeader "Access-Control-Allow-Origin" "*"
                           acc <- runDB $ selectList [ AddressStateRefBalance ==. n ] [LimitTo 100] :: Handler [Entity AddressStateRef]   
                           returnJson $ (P.map entityVal acc) 



