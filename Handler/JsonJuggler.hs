{-# LANGUAGE OverloadedStrings #-}


module Handler.JsonJuggler where

import Import

import Handler.Common 

import Data.Aeson
import qualified Blockchain.Data.DataDefs as DD
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import qualified Database.Esqueleto as E

import Data.List
import Debug.Trace

jsonBlk :: (ToJSON a, Monad m) => a -> m Value
jsonBlk a = returnJson a
