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

module Handler.AccountInfo where

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

getAccountInfoR :: Handler Value
getAccountInfoR = do
                 getParameters <- reqGetParams <$> getRequest

                 let offset = fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64
                 let index = maybe "0000000000000000000000000000000000000000" id $ extractValue "index" getParameters "0000000000000000000000000000000000000000"
                 let raw    = (fromIntegral $ (maybe 0 id $ extractPage "raw" getParameters) :: Integer) > 0

                 liftIO $ traceIO $ "parameters: " P.++ show getParameters
                 liftIO $ traceIO $ "index: " P.++ show index
                 -- liftIO $ traceIO $ "offset: " P.++ show offset

                 addHeader "Access-Control-Allow-Origin" "*"

                 addrs <- runDB $ E.selectDistinct $
                                        E.from $ \(accStateRef) -> do


                                        let criteria = P.map (getAccFilter (accStateRef)) $ getParameters 
                                        let allCriteria = ((accStateRef E.^. AddressStateRefAddress) E.>=. E.val (toAddr $ T.pack index)) : criteria

                                        E.where_ (P.foldl1 (E.&&.) allCriteria)

                                        -- E.offset $ (limit * offset)
                                        E.limit $ limit

                                        E.orderBy [E.asc (accStateRef E.^. AddressStateRefAddress)]
                                        --E.orderBy [E.desc (accStateRef E.^. AddressStateRefBalance)]

                                        return accStateRef


                 let modBlocks = (nub (P.map entityVal (addrs :: [Entity AddressStateRef])))
                 let newindex = pack $ (getAccNum $ P.last modBlocks) P.++ "0"
                 let extra p = P.zipWith extraFilter p (P.repeat (newindex))
                 -- this should actually use URL encoding code from Yesod
                 let next p = "/query/account?" P.++  (P.foldl1 (\a b -> (unpack a) P.++ "&" P.++ (unpack b)) $ P.map (\(k,v) -> (unpack k) P.++ "=" P.++ (unpack v)) (extra p))
                 let addedParam = appendIndex getParameters

                 toRet raw modBlocks (next addedParam) -- consider removing nub - it takes time n^{2}
             where
                 toRet raw bs gp = case if' raw bs (P.map asrToAsrPrime (P.zip (P.repeat gp) bs)) of 
                            Left a -> returnJson a
                            Right b -> returnJson b
                 limit = (fromIntegral $ fetchLimit :: Int64)
