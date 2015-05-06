{-# LANGUAGE OverloadedStrings #-}

module Handler.BlkCoinbase where
import Import


import Handler.Common

import Database.Persist       
import Database.Persist.TH
import Database.Persist.Postgresql
import qualified Prelude as P
import Blockchain.Data.Address
import Blockchain.ExtWord
import Numeric
import Blockchain.Data.DataDefs

import qualified Database.Esqueleto as E

import qualified Data.Text as T
import Data.List
import Handler.JsonJuggler
       


getBlkCoinbaseR :: Text -> Handler Value
getBlkCoinbaseR address = do
                   addHeader "Access-Control-Allow-Origin" "*"
                   blks <- runDB $ E.select $
                                        E.from $ \(blk `E.InnerJoin` bdRef) -> do

                                        E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )                                        
                                        E.where_ (( bdRef E.^. BlockDataRefCoinbase E.==. E.val (Address wd160)) )

                                        E.limit $ (fetchLimit)

                                        E.orderBy [E.desc (bdRef E.^. BlockDataRefNumber)]

                                        return blk
                   returnJson $ nub $ P.map bToBPrime (P.map entityVal (blks :: [Entity Block])) 

        where
          ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]


                    
