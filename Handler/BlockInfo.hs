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

import Handler.Common 
import Blockchain.Data.DataDefs
import Blockchain.Data.Address

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Numeric
import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util

import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BS

import qualified Database.Esqueleto as E
       
import Data.List
       
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Yesod.Core.Handler


getFilter::(E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity AddressStateRef), expr (Entity RawTransaction), expr (Entity Block))-> (Text, Text) -> expr (E.Value Bool)
getFilter (bdRef, accStateRef, rawTX, blk) ("number", v) = bdRef E.^. BlockDataRefNumber E.==. E.val (P.read $ T.unpack v :: Integer)
getFilter (bdRef, accStateRef, rawTX, blk) ("mingas", v) = bdRef E.^. BlockDataRefGasUsed E.>=. E.val (P.read $ T.unpack v :: Integer) 
getFilter (bdRef, accStateRef, rawTX, blk) ("minnum", v) = bdRef E.^. BlockDataRefNumber E.>=. E.val (P.read $ T.unpack v :: Integer)
getFilter (bdRef, accStateRef, rawTX, blk) ("coinbase", v) = bdRef E.^. BlockDataRefCoinbase E.==. E.val (Address wd160)
      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]
getFilter (bdRef, accStateRef, rawTX, blk) ("txaddress", v) = (rawTX E.^. RawTransactionBlockId E.==. blk E.^. BlockId)
                                                              E.&&. ((rawTX E.^. RawTransactionFromAddress E.==. E.val (Address wd160)))
                                                                      E.||. (rawTX E.^. RawTransactionToAddress E.==. E.val (Just (Address wd160)))
                                                              
      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]
-- getFilter (a, t) ("address", v) = t E.^. AddressStateRefAddress E.==. E.val (P.read $ T.unpack v)


getBlockInfoR :: Handler Value
getBlockInfoR = do
  	           getParameters <- reqGetParams <$> getRequest
                   addHeader "Access-Control-Allow-Origin" "*"
                   blks <- runDB $ E.select $
                                        E.from $ \(blk `E.InnerJoin` bdRef `E.FullOuterJoin` rawTX `E.LeftOuterJoin` accStateRef) -> do
                                        
                                        E.on ( accStateRef E.^. AddressStateRefAddress E.==. rawTX E.^. RawTransactionFromAddress )
                                        E.on ( rawTX E.^. RawTransactionBlockId E.==. bdRef E.^. BlockDataRefBlockId )
                                        E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )                                        


                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getFilter (bdRef, accStateRef, rawTX, blk)) $ getParameters ))

                                        E.limit $ (fetchLimit)

                                        E.orderBy [E.desc (bdRef E.^. BlockDataRefNumber)]

                                        return blk
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
