{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Handler.IncludeTransaction where

import Import

import Data.Aeson

import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import Data.ByteString as B

import Handler.JsonJuggler

import Blockchain.Data.DataDefs
import Blockchain.Data.Transaction
import Blockchain.Util
import Blockchain.Data.Address
import Blockchain.Data.Code

import qualified Prelude as P
import qualified Data.Text as T

postIncludeTransactionR :: Handler ()
postIncludeTransactionR = do
   tx <- parseJsonBody :: Handler (Result RawTransaction')
   case tx of
       (Success (RawTransaction' raw)) -> do
                _ <- runDB $ insert $ raw
                return ()
       (Error msg) -> do
                liftIO $ Import.putStrLn $ T.pack $ msg
                return ()                               
