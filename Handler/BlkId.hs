{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ScopedTypeVariables #-}


module Handler.BlkId where

import Import

import Handler.Common 

import Data.Aeson
import Blockchain.Data.DataDefs 
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import qualified Database.Esqueleto as E

import Data.List
       
import qualified Prelude as P

import Handler.JsonJuggler


getBlkIdR :: Integer -> Handler Value
getBlkIdR n = do addHeader "Access-Control-Allow-Origin" "*"

                 blks <- runDB $ E.select $ --Distinct
                         E.from $ \(a, t) -> do
                           E.where_ ( (a E.^. BlockDataRefBlockId E.==. E.val (toSqlKey (fromIntegral $ n)) ) E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)) ---- :: Handler [Entity AddressStateRef]
                           return a
                 returnJson $ nub $ P.map bdrToBdrPrime (P.map entityVal (blks :: [Entity BlockDataRef])) -- consider removing nub - it takes time n^{2}
