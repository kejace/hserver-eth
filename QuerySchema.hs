{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module QuerySchema
  ( Query (..),
    BlockQuery (..),
    AccountQuery (..),
    TransactionQuery (..)
  ) where

import Prelude
import Blockchain.Data.DataDefs
import Blockchain.Data.SignedTransaction
import Data.Aeson
import Data.Text
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics
import Data.Time.Clock

data Query =      BlkQuery BlockQuery |
                  AccQuery AccountQuery |
                  TxQuery TransactionQuery
               deriving (Show, Eq, Read, Generic)



data BlockQuery = -- ByHash { blockHash :: SHA } |
                  ByGas { gas :: Integer } |
                  ByGasRange { lowerGas :: Integer, upperGas :: Integer } |
                  -- ByTX { transaction :: SignedTransaction } |
                  ByTimestamp { timestamp :: UTCTime } |
                  ByTimestampRange { lowerTimestamp :: UTCTime, upperTimestamp :: UTCTime } |
                  ByNumber { number :: Integer } |
                  ByNumberRange { lowerNumber :: Integer, upperNumber :: Integer } |
                  ByDifficulty { difficulty :: Integer } |
                  ByDifficultyRange { lowerDifficulty :: Integer, upperDifficulty :: Integer }
              deriving (Show, Eq, Read, Generic)
                  
data AccountQuery = -- ByAddress { address :: Address } |
                    ByBalance { balance :: Integer } |
                    ByBalanceRange { lowerBalance :: Integer, upperBalance :: Integer } |
                    ByNonce { nonce :: Integer }  |
                    ByNonceRange { lowerNonce:: Integer, upperNonce :: Integer } -- |
                --    ByCodeHash { codeHash :: SHA }
              deriving (Show, Eq, Read, Generic)

data TransactionQuery = -- ByTXHash { txHash :: SHA } |
                        ByValue { value :: Integer } |
                        ByValueRange { lowerValue :: Integer, upperValue :: Integer } |
                        ByData { dat :: Integer } -- |
                        -- ByAddressTo Address |
                        -- ByAddressFrom Address
              deriving (Show, Eq, Read, Generic)

instance FromJSON Query
instance ToJSON Query

instance FromJSON BlockQuery
instance ToJSON BlockQuery

instance FromJSON AccountQuery
instance ToJSON AccountQuery

instance FromJSON TransactionQuery
instance ToJSON TransactionQuery

