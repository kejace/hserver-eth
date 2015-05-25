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
import Handler.JsonJuggler

import Control.Monad
import Data.Bool

import Handler.Filters

blockIdRef :: (E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity Block))-> expr (E.Value Bool)
blockIdRef (a, t) = (a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)
                    
if' :: Bool -> a -> b -> Either a b
if' x a b = if x == True then Left a else Right b

getBlockInfoR :: Handler Value
getBlockInfoR = do
                   getParameters <- reqGetParams <$> getRequest
                   
                   let offset = (fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64)
                   let index  = (fromIntegral $ (maybe 0 id $ extractPage "index" getParameters)  :: Integer)
                   let raw    = (fromIntegral $ (maybe 0 id $ extractPage "raw" getParameters) :: Integer) > 0

                   liftIO $ traceIO $ "parameters: " P.++ show getParameters
                   liftIO $ traceIO $ "index: " P.++ show index
                   liftIO $ traceIO $ "offset: " P.++ show offset
                   liftIO $ traceIO $ "raw: " P.++ show raw
                   
                   addHeader "Access-Control-Allow-Origin" "*"

                   blks <- runDB $ E.select $
                                        
                                        E.from $ \(blk `E.InnerJoin` bdRef `E.FullOuterJoin` rawTX `E.LeftOuterJoin` accStateRef) -> do
                                        
                                        E.on ( accStateRef E.^. AddressStateRefAddress E.==. rawTX E.^. RawTransactionFromAddress )
                                        E.on ( rawTX E.^. RawTransactionBlockId E.==. bdRef E.^. BlockDataRefBlockId )
                                        E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )                                        

                                        let criteria = P.map (getBlkFilter (bdRef, accStateRef, rawTX, blk)) $ getParameters 
                                        let allCriteria = ((bdRef E.^. BlockDataRefNumber) E.>=. E.val index) : criteria

                                        E.where_ (P.foldl1 (E.&&.) allCriteria)

                                        E.offset $ (limit * offset)
                                        E.limit $ limit

                                        E.orderBy [E.asc (bdRef E.^. BlockDataRefNumber)]

                                        return blk
                   liftIO $ traceIO $ "number of results: " P.++ (show $ P.length blks)
                   toRet raw $ nub (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
               where
                   toRet raw bs = case if' raw bs (P.map (bToBPrime) bs) of 
                              Left a -> returnJson a
                              Right b -> returnJson b
                   limit = (fromIntegral $ fetchLimit :: Int64)