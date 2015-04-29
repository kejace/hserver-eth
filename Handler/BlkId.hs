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

myQ :: (E.Esqueleto m expr backend) => (expr (Entity BlockDataRef), expr (Entity Block)) -> Integer -> m (expr (Entity BlockDataRef))
myQ (a, t) n = do
               E.where_  ( (a E.^. BlockDataRefNumber E.==. E.val n ) E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
               return a

getBlkIdR :: Integer -> Handler Value
getBlkIdR n = do addHeader "Access-Control-Allow-Origin" "*"

                 blks <- runDB $ E.select $ --Distinct
                         E.from $ \(a, t) -> do
                           E.where_ ( (a E.^. BlockDataRefNumber E.==. E.val n ) E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)) ---- :: Handler [Entity AddressStateRef]
                           return a
                 returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}