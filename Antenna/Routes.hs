{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Antenna.Routes where

import Antenna.App
import Antenna.Sync
import Antenna.Types
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Text                                     ( Text )
import Network.HTTP.Types
import Network.Wai 

data OkResponse = JsonOk (Maybe Value)
    deriving (Show)

instance ToJSON OkResponse where
    toJSON (JsonOk mb) = object $
        [ ("status", "success")
        , ("message", "OK") 
        ] ++ case mb of
              Nothing -> []
              Just b  -> [("body", b)]

data ErrorResponse = JsonError Text
    deriving (Show)

instance ToJSON ErrorResponse where
    toJSON (JsonError code) = object 
        [ ("status", "error") 
        , ("error", String code) ]

deleteNode :: Int -> WebM (AppState a) Network.Wai.Response
deleteNode node = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    liftIO $ atomically $ 
        writeTVar tvar as{ nodes = filter out $ nodes as }
    respondWith status200 $ JsonOk Nothing
  where
    out (_, n) = nodeId' n /= node

insertNode :: Text -> Node -> WebM (AppState a) Network.Wai.Response
insertNode name node = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    liftIO $ atomically $ 
        writeTVar tvar as{ nodes = (name, node):nodes as }
    respondWith status200 $ JsonOk Nothing

resetStack :: WebM (AppState a) Network.Wai.Response
resetStack = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    liftIO $ atomically $ 
        writeTVar tvar as{ transLog   = fromList [] 
                         , syncPoints = saturated $ syncPoints as }
    respondWith status200 $ JsonOk Nothing
  where
    saturated = map $ \(a, _) -> (a, Saturated)

getNodes :: WebM (AppState a) Network.Wai.Response
getNodes = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    respondWith status200 $ nodes as
 
data Transactions = Transactions
    { transactions :: [Action] 
    } deriving (Show)

instance ToJSON Transactions where
    toJSON (Transactions trans) = object
        [ "transactions" .= trans ]

data Collection a = Collection
    { collcnCount    :: Int
    , collcnTotal    :: Int
    , collcnResource :: a }

instance (ToJSON a) => ToJSON (Collection a) where
    toJSON Collection{..} = object
        [ "count"     .= collcnCount
        , "total"     .= collcnTotal 
        , "_embedded" .= collcnResource
        ]

getStack :: Int -> Int -> WebM (AppState a) Network.Wai.Response
getStack page pageSize = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    let log = transLog as
        res = toAscList (Proxy :: Proxy Timestamp) log
        collection = Transactions $ Prelude.take pageSize $ Prelude.drop (offs * pageSize) res
    respondWith status200 $ Collection pageSize (size log) collection
  where
    offs = pred page

runSyncRequest :: Request -> Int -> WebM (AppState a) Network.Wai.Response
runSyncRequest req nodeId = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    body <- liftIO $ strictRequestBody req
    case decode body of
        Just Commit{..} -> do
            let targetConsumers = lookupTargets (nodes as) targets
            resp <- processSyncRequest nodeId targetConsumers log syncPoint
            respondWith status200 resp
        _ -> respondWith status400 (JsonError "BAD_REQUEST")

respondWith :: (ToJSON b) => Status -> b -> WebM (AppState a) Network.Wai.Response
respondWith status = return . responseLBS status [("Content-type", "application/json")] . encode 

