{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Handler.PQuery where

import Import
import qualified QuerySchema as QS
import Data.Aeson
import qualified Blockchain.Data.DataDefs as DD
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql


import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString as B

import Blockchain.Data.Address
import Blockchain.SHA
import Blockchain.Data.SignedTransaction
import Blockchain.Util
import Blockchain.Database.MerklePatricia
       

share [ mkPersist sqlSettings ]
    DD.entityDefs

postPQueryR :: Handler Html
postPQueryR = do
                  query <- requireJsonBody :: Handler QS.Query
                  case query of
                       (QS.BlkQuery b) ->
                          case b of
                               (QS.ByGas g) ->
                                                  do blk <- runDB $ selectList [ BlockDataGasUsed ==. g ] [LimitTo 10] :: Handler [Entity BlockData]
                                                     defaultLayout $ [whamlet| #{show blk}|]
                
                               (QS.ByGasRange g1 g2) ->
                                                  do blk <- runDB $ selectList [ BlockDataGasUsed >=. g1 , BlockDataGasUsed <=. g2 ] [LimitTo 10] :: Handler [Entity BlockData]
                                                     defaultLayout $ [whamlet| #{show blk}|]
                
                               (QS.ByNumber n) ->
                                                  do blk <- runDB $ selectList [ BlockDataNumber ==. n ] [LimitTo 10] :: Handler [Entity BlockData]
                                                     defaultLayout $ [whamlet| #{show blk}|]
                
                               (QS.ByNumberRange n1 n2) ->
                                                  do blk <- runDB $ selectList [ BlockDataNumber >=. n1, BlockDataNumber <=. n2 ] [LimitTo 10] :: Handler [Entity BlockData]
                                                     defaultLayout $ [whamlet| #{show blk}|]
                
                               (QS.ByDifficulty d) ->
                                                  do blk <- runDB $ selectList [ BlockDataDifficulty ==. d ] [LimitTo 10] :: Handler [Entity BlockData]
                                                     defaultLayout $ [whamlet| #{show blk}|]
                
                               (QS.ByDifficultyRange d1 d2) ->
                                                  do blk <- runDB $ selectList [ BlockDataDifficulty >=. d1, BlockDataDifficulty <=. d2 ] [LimitTo 10] :: Handler [Entity BlockData]
                                                     defaultLayout $ [whamlet| #{show blk}|]
                
                               (QS.ByTimestamp t) ->
                                                  do blk <- runDB $ selectList [ BlockDataTimestamp ==. t ] [LimitTo 10] :: Handler [Entity BlockData]
                                                     defaultLayout $ [whamlet| #{show blk}|]
