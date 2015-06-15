module Handler.Stats where

import Import
import Yesod.Core.Handler

import Handler.Common
import Blockchain.Data.DataDefs

import Database.Persist       
import Database.Persist.TH
import Database.Persist.Postgresql
import qualified Prelude as P
import Blockchain.Data.Address
import Blockchain.Data.RawTransaction
import Blockchain.ExtWord
import Numeric

import qualified Data.Text as T

import qualified Database.Esqueleto as E

getStatsR :: Handler Html
getStatsR = do

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "BlockApps.net - statistics"
        $(widgetFile "stats")

getStatDiffR :: Handler Value
getStatDiffR  = do 
                   addHeader "Access-Control-Allow-Origin" "*"
                   blks <- runDB $ E.select $
                        E.from $ \(a, t) -> do
                        E.where_ (  a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)
                        let sum' = E.sum_ (a E.^. BlockDataRefDifficulty)
                        -- E.limit $ P.min (fromIntegral n :: Int64) fetchLimit 
                        -- E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
                        return sum'
                   return $ myval (blks :: [E.Value (Maybe Integer)]) 
            where
              myval ((E.Value (Just v)):vs) = object ["difficulty" .= (v :: Integer)]
              myval _                       = object ["difficulty" .= ("0" :: String)]

getStatTxR :: Handler Value
getStatTxR  = do 
                   addHeader "Access-Control-Allow-Origin" "*"
                   tx <- runDB $ E.select $
                                 E.from $ \rawTX -> do
                                 let count' = E.count (rawTX E.^. RawTransactionBlockId)
                                 return count'
                   return $ myval (tx :: [E.Value Integer]) 
            where
              myval ((E.Value v):vs) = object ["transactionCount" .= (v :: Integer)]
              myval _                       = object ["transactionCount" .= ("0" :: String)]              