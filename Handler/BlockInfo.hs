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

import Data.Aeson
import qualified Blockchain.Data.DataDefs as DD
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util

import Data.ByteString.Base16 as B16
import Data.ByteString.Lazy as BS

import qualified Database.Esqueleto as E
       
import Handler.PQuery
import Data.List
       
import qualified Prelude as P
import qualified Data.Text.Encoding as T

import Yesod.Core.Handler


getBlockInfoR :: Handler Html
getBlockInfoR = do addHeader "Access-Control-Allow-Origin" "*"
		   defaultLayout [whamlet| placeholder for refactor |]
{-
qry :: E.SqlQuery (Entity Block)

sd :: SideData
-}
