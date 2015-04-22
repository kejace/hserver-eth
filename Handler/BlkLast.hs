module Handler.BlkLast where

import Import

import Handler.Common (fetchLimit)

import Data.Aeson
import qualified Blockchain.Data.DataDefs as DD
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import qualified Database.Esqueleto as E

import Handler.PQuery
import Data.List


import Handler.Common (fetchLimit)
       
import qualified Prelude as P

getBlkLastR :: Integer -> Handler Value
getBlkLastR n      =            do addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.select $
                                        E.from $ \(a, t) -> do
                                        E.where_ (  a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)
                                        E.limit $ P.min (fromIntegral n :: Int64) fetchLimit 
                                        E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
                                        return t
                                   returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}
                                
