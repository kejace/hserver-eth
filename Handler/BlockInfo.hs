{-# LANGUAGE DeriveDataTypeable
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , GADTs
 #-}

module Handler.BlockInfo where

import Import

import Handler.Common (fetchLimit)

import Data.Aeson
import qualified Blockchain.Data.DataDefs as DD
import qualified Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util

import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BS

import qualified Database.Esqueleto as E
       
import Handler.PQuery
import Data.List
       
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Yesod.Core.Handler


getFilter::(E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity AddressStateRef))-> (Text, Text) -> expr (E.Value Bool)
getFilter (a, t) ("number", v) = a E.^. BlockDataRefNumber E.==. E.val (P.read $ T.unpack v :: Integer)
getFilter (a, t) ("mingas", v) = a E.^. BlockDataRefGasUsed E.>=. E.val (P.read $ T.unpack v :: Integer) 
getFilter (a, t) ("minnum", v) = a E.^. BlockDataRefNumber E.>=. E.val (P.read $ T.unpack v :: Integer)
-- getFilter (a, t) ("address", v) = t E.^. AddressStateRefAddress E.==. E.val (P.read $ T.unpack v)


getBlockInfoR :: Handler Value
getBlockInfoR = do
  	           getParameters <- reqGetParams <$> getRequest
                   addHeader "Access-Control-Allow-Origin" "*"
                   blks <- runDB $ E.select $
                                        E.from $ \(a, t) -> do
                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getFilter (a, t)) $ getParameters ))
                                        E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
                                        E.limit $ fetchLimit
                                        return t
                   returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}


{-

do
  addHeader "Access-Control-Allow-Origin" "*"
  blks <- runDB $ E.selectDistinct $
                                        E.from $ \x@(_, t) -> do
                                        E.where_ (
                                          P.foldl1 (E.&&.)     [getFilter x "number", getFilter x "blockId"])
                                        return t
  returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}
-}
