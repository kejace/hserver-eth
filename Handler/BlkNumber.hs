{-# LANGUAGE OverloadedStrings #-}


module Handler.BlkNumber where

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
       
getBlkNumberR :: Integer -> Handler Value
getBlkNumberR n      = do addHeader "Access-Control-Allow-Origin" "*"
                          blkD <- runDB $ selectList [ BlockDataNumber ==. n ] [LimitTo 1] :: Handler [Entity BlockData]   
                          blk <- runDB $ selectList [ BlockBlockData ==.  (P.head $ P.map entityVal blkD)  ] [LimitTo 1] :: Handler [Entity Block]
                          returnJson $ P.head $ (P.map entityVal blk) 
