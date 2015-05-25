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

getDevices :: WebM (AppState a) Network.Wai.Response
getDevices = undefined

getStack :: WebM (AppState a) Network.Wai.Response
getStack = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    let res = toList $ transLog as
    respondWith status200 res

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

