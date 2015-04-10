module Handler.BlkGas where

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


        
getBlkGasR :: Integer -> Handler Value
getBlkGasR g      = do  addHeader "Access-Control-Allow-Origin" "*"
                        blks <- runDB $ E.select $
                                    E.from $ \(a, t) -> do
                                    E.where_ ( (a E.^. BlockDataRefGasUsed E.==. E.val g)  E.&&.( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
                                    E.limit $ 100
                                    E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
                                    return t
                        returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}
