module Handler.BlkNumberRange where

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
       
import qualified Prelude as P
       
getFilter :: (E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity Block))-> (Integer, Integer) -> expr (E.Value Bool)
getFilter (bdr, block) (n1, n2) = ( (bdr E.^. BlockDataRefNumber E.>=. E.val n1 ) E.&&. (bdr E.^. BlockDataRefNumber E.<=. E.val n2)  E.&&. ( bdr E.^. BlockDataRefBlockId E.==. block E.^. BlockId))

getBlkNumberRangeR :: Integer -> Integer -> Handler Value
getBlkNumberRangeR n1 n2      = do addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.select $
                                        E.from $ \(bdr, block) -> do
                                      --  E.where_ ( (a E.^. BlockDataRefNumber E.>=. E.val n1 ) E.&&. (a E.^. BlockDataRefNumber E.<=. E.val n2)  E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
                                        E.where_ $ getFilter (bdr, block) (n1, n2)
                                        E.orderBy [E.asc (bdr E.^. BlockDataRefNumber)]
                                        E.limit $ fetchLimit
                                        return block
                                   returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}
