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

import Data.Aeson
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

--makeFilter :: (E.Esqueleto query expr backend) => (expr (Entity BlockDataRef) -> (expr (Entity BlockDataRef)) -> a -> (Text, Text) -> expr (E.Value Bool)


getFilter::(E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef))-> (Text, Text) -> expr (E.Value Bool)
getFilter a ("num", v)    = a E.^. BlockDataRefNumber  E.==. E.val (P.read $ T.unpack v :: Integer)
getFilter a ("minnum", v) = a E.^. BlockDataRefNumber  E.>=. E.val (P.read $ T.unpack v :: Integer) 
getFilter a ("maxnum", v) = a E.^. BlockDataRefNumber  E.<=. E.val (P.read $ T.unpack v :: Integer) 
getFilter a ("gasused", v)    = a E.^. BlockDataRefGasUsed E.==. E.val (P.read $ T.unpack v :: Integer) 
getFilter a ("mingasused", v) = a E.^. BlockDataRefGasUsed E.>=. E.val (P.read $ T.unpack v :: Integer)  
getFilter a ("maxgasused", v) = a E.^. BlockDataRefGasUsed E.<=. E.val (P.read $ T.unpack v :: Integer) 
getFilter a ("diff", v)    = a E.^. BlockDataRefDifficulty E.==. E.val (P.read $ T.unpack v :: Integer) 
getFilter a ("mindiff", v) = a E.^. BlockDataRefDifficulty E.>=. E.val (P.read $ T.unpack v :: Integer) 
getFilter a ("maxdiff", v) = a E.^. BlockDataRefDifficulty E.<=. E.val (P.read $ T.unpack v :: Integer) 
getFilter a ("gaslimit", v)    = a E.^. BlockDataRefGasLimit E.==. E.val (P.read $ T.unpack v :: Integer) 
getFilter a ("mingaslimit", v) = a E.^. BlockDataRefGasLimit E.>=. E.val (P.read $ T.unpack v :: Integer) 
getFilter a ("maxgaslimit", v) = a E.^. BlockDataRefGasLimit E.<=. E.val (P.read $ T.unpack v :: Integer)
getFilter a ("time", v) = a E.^. BlockDataRefTimestamp E.==. E.val utctime
  where utctime = stringToDate v
getFilter a ("mintime", v) = a E.^. BlockDataRefTimestamp E.>=. E.val utctime
  where utctime = stringToDate v
getFilter a ("maxtime", v) = a E.^. BlockDataRefTimestamp E.<=. E.val utctime
  where utctime = stringToDate v
getFilter a ("coinbase", v) = a E.^. BlockDataRefCoinbase E.==. E.val (Address wd160)
  where ((wd160, _):_) = readHex $ T.unpack $ (P.read $ ("\"" P.++ (T.unpack v) P.++ "\"") ):: [(Word160,String)]


blockIdRef :: (E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity Block))-> expr (E.Value Bool)
blockIdRef (a, t) = (a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)

getBlockInfoR :: Handler Value
getBlockInfoR = do
  	           getParameters <- reqGetParams <$> getRequest
                   liftIO $ traceIO $ show getParameters
                   addHeader "Access-Control-Allow-Origin" "*"
                   blks <- runDB $ E.select $
                          E.from $ \(blockdataref, block) -> do
                            E.where_ $ (P.foldl1 (E.&&.) $ (P.map (getFilter blockdataref) $ getParameters) ) E.&&. blockIdRef (blockdataref, block)
                            E.orderBy [E.desc (blockdataref E.^. BlockDataRefNumber)]
                            E.limit $ fetchLimit
                            return blockdataref
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
