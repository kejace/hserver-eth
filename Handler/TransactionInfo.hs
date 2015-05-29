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

                 let offset = (fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64)
                 let index = (fromIntegral $ (maybe 0 id $ extractPage "index" getParameters)  :: Integer)
                   
                 liftIO $ traceIO $ "parameters: " P.++ show getParameters
                 liftIO $ traceIO $ "index: " P.++ show index
                 liftIO $ traceIO $ "offset: " P.++ show offset

                 addHeader "Access-Control-Allow-Origin" "*"
                 txs <- runDB $ E.select $
                                        E.from $ \(rawTx) -> do
                        
                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getTransFilter (rawTx)) $ getParameters ))

                                        -- let criteria = P.map (getBlkFilter (bdRef, accStateRef, rawTX, blk)) $ getParameters 
                                        -- let allCriteria = ((bdRef E.^. BlockDataRefNumber) E.>=. E.val index) : criteria

                                        -- E.where_ (P.foldl1 (E.&&.) allCriteria)

                                        -- E.offset $ (limit * offset)
                                        -- E.limit $ limit

                                        -- E.orderBy [E.asc (bdRef E.^. BlockDataRefNumber)]

                                        E.offset $ (limit * offset)
                                        E.limit $ (limit)
                                        E.orderBy [E.desc (rawTx E.^. RawTransactionBlockId)]

                                        return rawTx
                 liftIO $ traceIO $ "number of results: " P.++ (show $ P.length txs)
                 returnJson $ nub $ P.map rtToRtPrime (P.map id (P.map entityVal (txs :: [Entity RawTransaction]))) -- consider removing nub - it takes time n^{2}
                 where 
                    limit = (fromIntegral $ fetchLimit :: Int64)
