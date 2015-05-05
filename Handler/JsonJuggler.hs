{-# LANGUAGE OverloadedStrings #-}

module Handler.JsonJuggler where

import Import

import Handler.Common 
import Blockchain.Data.DataDefs
import Blockchain.Data.Address
import Blockchain.Data.PersistTypes
import Blockchain.Data.Transaction

import Data.Aeson
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Data.ByteString

import qualified Database.Esqueleto as E

import Numeric
import Data.List
import Debug.Trace

import Data.Maybe

import Prelude as P

jsonBlk :: (ToJSON a, Monad m) => a -> m Value
jsonBlk a = returnJson a


data RawTransaction' = RawTransaction' RawTransaction deriving (Eq, Show)

instance ToJSON RawTransaction' where
    toJSON (RawTransaction' rt@(RawTransaction (Address fa) non gp gl ta val cod v r s bid)) =
        object ["from" .= showHex fa "", "nonce" .= non, "gasPrice" .= gp, "gasLimit" .= gl,
        "to" .= fmap show ta , "value" .= val, "codeOrData" .= cod, "v" .= v, "r" .= r, "s" .= s,
        "blockId" .= bid, "transactionType" .= (show $ rawTransactionSemantics rt)]

rtToRtPrime :: RawTransaction -> RawTransaction'
rtToRtPrime x = RawTransaction' x

data Transaction' = Transaction' Transaction deriving (Eq, Show)

instance ToJSON Transaction' where
    toJSON (Transaction' (MessageTX tnon tgp tgl tto tval td tr ts tv)) = 
        object ["nonce" .= tnon, "gasPrice" .= tgp, "gasLimit" .= tgl, "to" .= tto, "value" .= tval,
        "data" .= td, "r" .= tr, "s" .= ts, "v" .= tv, "transactionType" .= (show FunctionCall)]
    toJSON (Transaction' (ContractCreationTX tnon tgp tgl tval ti tr ts tv)) = 
        object ["nonce" .= tnon, "gasPrice" .= tgp, "gasLimit" .= tgl, "value" .= tval, "init" .= ti,
        "r" .= tr, "s" .= ts, "v" .= tv, "transactionType" .= (show Contract)]

tToTPrime :: Transaction -> Transaction'
tToTPrime x = Transaction' x

data Block' = Block' Block deriving (Eq, Show)

instance ToJSON Block' where
      toJSON (Block' (Block bd rt bu)) = --"hello"
        object ["blockData" .= bdToBdPrime bd,
         "receiptTransactions" .= P.map tToTPrime rt,
         "blockUncles" .= P.map bdToBdPrime bu]

bToBPrime :: Block -> Block'
bToBPrime x = Block' x

data BlockData' = BlockData' BlockData deriving (Eq, Show)

instance ToJSON BlockData' where
      toJSON (BlockData' (BlockData ph uh cb@(Address a) sr tr rr lb d num gl gu ts ed non mh)) = 
        object ["parentHash" .= ph, "unclesHash" .= uh, "coinbase" .= (showHex a ""), "stateRoot" .= sr,
        "transactionsRoot" .= tr, "receiptsRoot" .= rr, "difficulty" .= d, "number" .= num,
        "gasLimit" .= gl, "gasUsed" .= gu, "timestamp" .= ts, "extraData" .= ed, "nonce" .= non,
        "mixHash" .= mh]

bdToBdPrime :: BlockData -> BlockData'
bdToBdPrime x = BlockData' x

data BlockDataRef' = BlockDataRef' BlockDataRef deriving (Eq, Show)

instance ToJSON BlockDataRef' where
      toJSON (BlockDataRef' (BlockDataRef ph uh cb@(Address a) sr tr rr lb d num gl gu ts ed non mh bi h)) = 
        object ["parentHash" .= ph, "unclesHash" .= uh, "coinbase" .= (showHex a ""), "stateRoot" .= sr,
        "transactionsRoot" .= tr, "receiptsRoot" .= rr, "difficulty" .= d, "number" .= num,
        "gasLimit" .= gl, "gasUsed" .= gu, "timestamp" .= ts, "extraData" .= ed, "nonce" .= non,
        "mixHash" .= mh, "blockId" .= bi, "hash" .= h]


bdrToBdrPrime :: BlockDataRef -> BlockDataRef'
bdrToBdrPrime x = BlockDataRef' x

data AddressStateRef' = AddressStateRef' AddressStateRef deriving (Eq, Show)

instance ToJSON AddressStateRef' where
    toJSON (AddressStateRef' (AddressStateRef a@(Address x) n b cr ch)) = 
        object ["address" .= (showHex x ""), "nonce" .= n, "balance" .= b, 
        "contractRoot" .= cr, "codeHash" .= ch]

asrToAsrPrime :: AddressStateRef -> AddressStateRef'
asrToAsrPrime x = AddressStateRef' x

--jsonFix x@(AddressStateRef a b c d e) = AddressStateRef' x
--jsonFix x@(BlockDataRef a b c d e f g h i j k l m n o p q) = BlockDataRef' x


data Address' = Address' Address deriving (Eq, Show)
adToAdPrime x = Address' x

--instance ToJSON Address' where
--  toJSON (Address' x) = object [ "address" .= (showHex x "") ]

data TransactionType = Contract | FunctionCall | Transfer deriving (Eq, Show)

--instance ToJSON TransactionType where 
--   toJSON x = object ["transactionType" .= show x]

transactionSemantics :: Transaction -> TransactionType
transactionSemantics t@(MessageTX tnon tgp tgl tto@(Address x) tval td tr ts tv) = work
    where work | P.length (showHex x "") > 0      = FunctionCall
               | P.length (showHex x "") == 0     = FunctionCall
transactionSemantics t@(ContractCreationTX tnon tgp tgl tval ti tr ts tv) = Contract

isAddr :: Maybe Address -> Bool
isAddr a = case a of
      Just x   -> True
      Nothing  -> False

rawTransactionSemantics :: RawTransaction -> TransactionType
rawTransactionSemantics t@(RawTransaction fa non gp gl ta val cod v r s bid) = work
     where work | (not (isAddr ta)) && ((Data.ByteString.length cod) > 0)   = Contract
                | (isAddr ta) &&  ((Data.ByteString.length cod) > 0)        = FunctionCall
                | otherwise = Transfer
