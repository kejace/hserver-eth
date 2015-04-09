module Handler.BlkGasRange where

import Import


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
       
getBlkGasRangeR :: Integer -> Integer -> Handler Value
getBlkGasRangeR g1 g2      =    do addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.selectDistinct $
                                        E.from $ \(a, t) -> do
                                        E.where_ ( (a E.^. BlockDataRefGasUsed E.>=. E.val g1 ) E.&&. (a E.^. BlockDataRefGasUsed E.<=. E.val g2)  E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
                                        return t
                                   returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}