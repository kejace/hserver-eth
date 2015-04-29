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

import Control.Monad

import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import System.Locale
import Data.Time
import Data.Time.Format


import Blockchain.Data.Address
import Blockchain.ExtWord
import Numeric

import Yesod.Core.Handler

import Debug.Trace

getFilter::(E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity AddressStateRef), expr (Entity RawTransaction), expr (Entity Block))-> (Text, Text) -> expr (E.Value Bool)

getFilter (bdRef, accStateRef, rawTX, blk) ("number", v)    = bdRef E.^. BlockDataRefNumber E.==. E.val (P.read $ T.unpack v :: Integer)
getFilter (bdRef, accStateRef, rawTX, blk) ("minnum", v)    = bdRef E.^. BlockDataRefNumber E.>=. E.val (P.read $ T.unpack v :: Integer)
getFilter (bdRef, accStateRef, rawTX, blk) ("maxnum", v)    = bdRef E.^. BlockDataRefNumber E.<=. E.val (P.read $ T.unpack v :: Integer)

getFilter (bdRef, accStateRef, rawTX, blk) ("gas", v)       = bdRef E.^. BlockDataRefGasUsed E.==. E.val (P.read $ T.unpack v :: Integer) 
getFilter (bdRef, accStateRef, rawTX, blk) ("mingas", v)    = bdRef E.^. BlockDataRefGasUsed E.>=. E.val (P.read $ T.unpack v :: Integer) 
getFilter (bdRef, accStateRef, rawTX, blk) ("maxgas", v)    = bdRef E.^. BlockDataRefGasUsed E.<=. E.val (P.read $ T.unpack v :: Integer) 

getFilter (bdRef, accStateRef, rawTX, blk) ("gaslim", v)    = bdRef E.^. BlockDataRefGasLimit E.==. E.val (P.read $ T.unpack v :: Integer) 
getFilter (bdRef, accStateRef, rawTX, blk) ("mingaslim", v) = bdRef E.^. BlockDataRefGasLimit E.>=. E.val (P.read $ T.unpack v :: Integer) 
getFilter (bdRef, accStateRef, rawTX, blk) ("maxgaslim", v) = bdRef E.^. BlockDataRefGasLimit E.<=. E.val (P.read $ T.unpack v :: Integer) 

getFilter (bdRef, accStateRef, rawTX, blk) ("diff", v)      = bdRef E.^. BlockDataRefDifficulty E.==. E.val (P.read $ T.unpack v :: Integer) 
getFilter (bdRef, accStateRef, rawTX, blk) ("mindiff", v)   = bdRef E.^. BlockDataRefDifficulty E.>=. E.val (P.read $ T.unpack v :: Integer) 
getFilter (bdRef, accStateRef, rawTX, blk) ("maxdiff", v)   = bdRef E.^. BlockDataRefDifficulty E.<=. E.val (P.read $ T.unpack v :: Integer) 

getFilter (bdRef, accStateRef, rawTX, blk) ("time", v)      = bdRef E.^. BlockDataRefTimestamp E.==. E.val (stringToDate v)
getFilter (bdRef, accStateRef, rawTX, blk) ("mintime", v)   = bdRef E.^. BlockDataRefTimestamp E.>=. E.val (stringToDate v)
getFilter (bdRef, accStateRef, rawTX, blk) ("maxtime", v)   = bdRef E.^. BlockDataRefTimestamp E.<=. E.val (stringToDate v)

getFilter (bdRef, accStateRef, rawTX, blk) ("txaddress", v) = rawTX E.^. RawTransactionBlockId E.==. blk E.^. BlockId

--getFilter (bdRef, accStateRef, rawTX, blk) ("coinbase", v)  = bdRef E.^. BlockDataRefCoinbase E.==. E.val (Address wd160)
--      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]

getFilter (bdRef, accStateRef, rawTX, blk) ("coinbase", v) = bdRef E.^. BlockDataRefCoinbase E.==. E.val (Address wd160)
      where ((wd160, _):_) = readHex $ T.unpack $ (P.read $ ("\"" P.++ (T.unpack v) P.++ "\"") ):: [(Word160,String)]


blockIdRef :: (E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity Block))-> expr (E.Value Bool)
blockIdRef (a, t) = (a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)

getBlockInfoR :: Handler Value
getBlockInfoR = do
  	           getParameters <- reqGetParams <$> getRequest
                   liftIO $ traceIO $ show getParameters
                   addHeader "Access-Control-Allow-Origin" "*"
                   blks <- runDB $ E.select $
                                        E.from $ \(bdRef `E.InnerJoin` blk `E.InnerJoin` rawTX `E.LeftOuterJoin` accStateRef) -> do
                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getFilter (bdRef, accStateRef, rawTX, blk)) $ getParameters ))
                                        E.on ( accStateRef E.^. AddressStateRefAddress E.==. rawTX E.^. RawTransactionFromAddress )
                                        E.on ( rawTX E.^. RawTransactionBlockId E.==. blk E.^. BlockId )
                                        E.on ( blk E.^. BlockId E.==. bdRef E.^. BlockDataRefBlockId )
                                        

                                        E.limit $ fetchLimit
                                        
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
