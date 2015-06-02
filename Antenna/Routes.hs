{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Antenna.Routes 
    ( OkResponse(..)
    , ErrorResponse(..)
    , getStack
    , resetStack
    , getNodes
    , deleteNode
    , updateNode
    , insertNode
    , respondWith
    , runSyncRequest
    ) where

import Antenna.App
import Antenna.Sync
import Antenna.Types
import Control.Arrow                                 ( second )
import Control.Applicative                           ( (<$>) )
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Text                                     ( Text, pack )
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

data Payload a = Payload String [a]

instance (ToJSON a) => ToJSON (Payload a) where
    toJSON (Payload key _data) = 
        object [ pack key .= _data ]

data Collection a = Collection
    { collcnCount    :: Int
    , collcnTotal    :: Int
    , collcnResource :: Payload a }

instance (ToJSON a) => ToJSON (Collection a) where
    toJSON Collection{..} = object
        [ "count"     .= collcnCount
        , "total"     .= collcnTotal 
        , "_embedded" .= collcnResource
        ]

deleteNode :: Int -> WebM (AppState a) Network.Wai.Response
deleteNode node = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    liftIO $ atomically $ 
        writeTVar tvar as{ nodes = filter out $ nodes as }
    respondWith status200 $ JsonOk Nothing
  where
    out (_,n) = nodeId' n /= node

updateNode :: Int -> Text -> [Int] -> WebM (AppState a) Network.Wai.Response
updateNode _id name _candidates = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    case lookupNode _id (nodes as) of
      Nothing -> respondWith status500 (JsonError "INTERNAL_SERVER_ERROR")
      Just node -> do
        let node' = node{ candidates = _candidates }
        liftIO $ atomically $ 
            writeTVar tvar as{ nodes = (name, node'):filter out (nodes as) }
        respondWith status200 $ JsonOk Nothing
  where
    out (_,n) = nodeId' n /= _id

insertNode :: Text -> Node -> WebM (AppState a) Network.Wai.Response
insertNode name node = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    let nodeIds  = nodeId' . snd <$> nodes as
        node'    = node{ candidates = nodeIds }
        newNodes = insertNodeId <$> nodes as
    liftIO $ atomically $ writeTVar tvar as{ nodes = (name, node'):newNodes }
    respondWith status200 $ JsonOk Nothing
  where
    insertNodeId = second $ \n -> n{ candidates = nodeId' node:candidates n}

resetStack :: WebM (AppState a) Network.Wai.Response
resetStack = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    liftIO $ atomically $ 
        writeTVar tvar as{ transLog   = fromList [] 
                         , syncPoints = saturated $ syncPoints as }
    respondWith status200 $ JsonOk Nothing
  where
    saturated = map $ \(a,_) -> (a, Saturated)

data NodeObj = NodeObj
    { _nodeId      :: Int
    , _nodeName    :: Text
    , _nodeType    :: NodeType 
    , _nodeLocked  :: Bool 
    , _nodeTargets :: [Int]
    } deriving (Show)

instance ToJSON NodeObj where
    toJSON NodeObj{..} = object 
        [ "id"      .= _nodeId
        , "name"    .= _nodeName
        , "type"    .= _nodeType 
        , "locked"  .= _nodeLocked 
        , "targets" .= _nodeTargets ]

toObj :: (Text, Node) -> NodeObj
toObj (t, n) = NodeObj (nodeId' n) t (nodeType n) (locked n) (candidates n)

getNodes :: WebM (AppState a) Network.Wai.Response
getNodes = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    let nodes' = toObj <$> nodes as
        len = length nodes'
    respondWith status200 $ Collection len len (Payload "nodes" nodes')
 
getStack :: Int -> Int -> WebM (AppState a) Network.Wai.Response
getStack page pageSize = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    let log = transLog as
        logSize = size log
        res = toDescList (Proxy :: Proxy (Timestamp, BatchIndex)) log
        collection = Prelude.take pageSize $ Prelude.drop (offs * pageSize) res
    respondWith status200 $ Collection (min logSize pageSize) logSize (Payload "transactions" collection)
  where
    offs = pred page

runSyncRequest :: Request -> Int -> WebM (AppState a) Network.Wai.Response
runSyncRequest req nodeId = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    body <- liftIO $ strictRequestBody req
    case (lookupNode nodeId $ nodes as, decode body) of
        (Just node', Just Commit{..}) -> do
            let targetConsumers = lookupTargets (nodes as) targets
            resp <- processSyncRequest node' targetConsumers log syncPoint
            respondWith status200 resp
        _ -> respondWith status400 (JsonError "BAD_REQUEST")

respondWith :: (ToJSON b) => Status -> b -> WebM (AppState a) Network.Wai.Response
respondWith status = return . responseLBS status [("Content-type", "application/json")] . encode 

