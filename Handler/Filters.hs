module Handler.Filters where

import qualified Database.Esqueleto as E

import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Numeric
import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util

import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.Data.DataDefs
import Blockchain.Data.Address


import Import

getBlkFilter::(E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity AddressStateRef), expr (Entity RawTransaction), expr (Entity Block))-> (Text, Text) -> expr (E.Value Bool)

getBlkFilter (bdRef, accStateRef, rawTX, blk) ("number", v)    = bdRef E.^. BlockDataRefNumber E.==. E.val (P.read $ T.unpack v :: Integer)
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("minnum", v)    = bdRef E.^. BlockDataRefNumber E.>=. E.val (P.read $ T.unpack v :: Integer)
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("maxnum", v)    = bdRef E.^. BlockDataRefNumber E.<=. E.val (P.read $ T.unpack v :: Integer)

getBlkFilter (bdRef, accStateRef, rawTX, blk) ("gas", v)       = bdRef E.^. BlockDataRefGasUsed E.==. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("mingas", v)    = bdRef E.^. BlockDataRefGasUsed E.>=. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("maxgas", v)    = bdRef E.^. BlockDataRefGasUsed E.<=. E.val (P.read $ T.unpack v :: Integer) 

getBlkFilter (bdRef, accStateRef, rawTX, blk) ("gaslim", v)    = bdRef E.^. BlockDataRefGasLimit E.==. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("mingaslim", v) = bdRef E.^. BlockDataRefGasLimit E.>=. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("maxgaslim", v) = bdRef E.^. BlockDataRefGasLimit E.<=. E.val (P.read $ T.unpack v :: Integer) 

getBlkFilter (bdRef, accStateRef, rawTX, blk) ("diff", v)      = bdRef E.^. BlockDataRefDifficulty E.==. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("mindiff", v)   = bdRef E.^. BlockDataRefDifficulty E.>=. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("maxdiff", v)   = bdRef E.^. BlockDataRefDifficulty E.<=. E.val (P.read $ T.unpack v :: Integer) 

getBlkFilter (bdRef, accStateRef, rawTX, blk) ("time", v)      = bdRef E.^. BlockDataRefTimestamp E.==. E.val (stringToDate v)
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("mintime", v)   = bdRef E.^. BlockDataRefTimestamp E.>=. E.val (stringToDate v)
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("maxtime", v)   = bdRef E.^. BlockDataRefTimestamp E.<=. E.val (stringToDate v)

getBlkFilter (bdRef, accStateRef, rawTX, blk) ("txaddress", v) = (rawTX E.^. RawTransactionBlockId E.==. blk E.^. BlockId)
                                                              E.&&. ((rawTX E.^. RawTransactionFromAddress E.==. E.val (Address wd160)))
                                                                      E.||. (rawTX E.^. RawTransactionToAddress E.==. E.val (Just (Address wd160)))                                                      
      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]

getBlkFilter (bdRef, accStateRef, rawTX, blk) ("coinbase", v) = bdRef E.^. BlockDataRefCoinbase E.==. E.val (Address wd160)
      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]


getBlkFilter (bdRef, accStateRef, rawTX, blk) ("address", v)   = accStateRef E.^. AddressStateRefAddress E.==. E.val (Address wd160)
      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]

getAccFilter (accStateRef) ("balance", v)      = accStateRef E.^. AddressStateRefBalance E.==. E.val (P.read $ T.unpack v :: Integer) 
getAccFilter (accStateRef) ("minbalance", v)   = accStateRef E.^. AddressStateRefBalance E.>=. E.val (P.read $ T.unpack v :: Integer) 
getAccFilter (accStateRef) ("maxbalance", v)   = accStateRef E.^. AddressStateRefBalance E.<=. E.val (P.read $ T.unpack v :: Integer) 

getAccFilter (accStateRef) ("nonce", v)        = accStateRef E.^. AddressStateRefNonce E.==. E.val (P.read $ T.unpack v :: Integer)
getAccFilter (accStateRef) ("minnonce", v)     = accStateRef E.^. AddressStateRefNonce E.>=. E.val (P.read $ T.unpack v :: Integer)
getAccFilter (accStateRef) ("maxnonce", v)     = accStateRef E.^. AddressStateRefNonce E.<=. E.val (P.read $ T.unpack v :: Integer)

getAccFilter (accStateRef) ("address", v)      = accStateRef E.^. AddressStateRefAddress E.==. E.val (Address wd160)
      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]

getTransFilter (rawTx)     ("address", v)    = rawTx E.^. RawTransactionFromAddress E.==. E.val (Address wd160) E.||. rawTx E.^. RawTransactionToAddress E.==. E.val (Just (Address wd160))
      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]

getTransFilter (rawTx)     ("from", v)    = rawTx E.^. RawTransactionFromAddress E.==. E.val (Address wd160)
      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]

getTransFilter (rawTx)     ("to", v)    = rawTx E.^. RawTransactionToAddress E.==. E.val (Just (Address wd160))
      where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]