module Handler.Query where

import Import
import qualified QuerySchema as QS
import Data.Aeson
import Blockchain.Data.DataDefs
import Data.ByteString.Lazy as BS

{-
queryGen :: QS.Query -> SQL Action
-}

getQueryR :: Handler Value
getQueryR = do
                query <- requireJsonBody :: Handler QS.Query
                return $ object ["reply" .= String "Got your Message!"]
                
                -- 

             {-   case query of
                   (QS.BlkQuery b) ->
                              case b of
                                  (QS.ByNumber n) -> do
                                                        blk <- runDB $ selectList [ blockDataNumber ==. n] [LimitTo 1]
                                                        return blk
                                                      
                                  _ -> return
                   _ -> return                
-}
