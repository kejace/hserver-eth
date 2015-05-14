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


module Handler.TransactionInfo where

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
import Handler.JsonJuggler

import Handler.Filters

getTransactionInfoR :: Handler Value
getTransactionInfoR = do
                 getParameters <- reqGetParams <$> getRequest
                 liftIO $ traceIO $ show getParameters
                 let offset = (fromIntegral $ (maybe 0 id $ extractPage getParameters)  :: Int64)
                 addHeader "Access-Control-Allow-Origin" "*"
                 addrs <- runDB $ E.select $
                                        -- E.from $ \(blk `E.InnerJoin` bdRef `E.FullOuterJoin` rawTX `E.LeftOuterJoin` accStateRef) -> do
                        
                                        -- E.on ( accStateRef E.^. AddressStateRefAddress E.==. rawTX E.^. RawTransactionFromAddress )
                                        -- E.on ( rawTX E.^. RawTransactionBlockId E.==. bdRef E.^. BlockDataRefBlockId )
                                        -- E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )    
                                        E.from $ \(rawTx) -> do
                        
                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getTransFilter (rawTx)) $ getParameters ))

                                        E.offset $ (limit * offset)
                                        E.limit $ (limit)
                                        E.orderBy [E.desc (rawTx E.^. RawTransactionNonce)]

                                        return rawTx
                 returnJson $ nub $ P.map rtToRtPrime (P.map id (P.map entityVal (addrs :: [Entity RawTransaction]))) -- consider removing nub - it takes time n^{2}
                 where 
                    limit = (fromIntegral $ fetchLimit :: Int64)
