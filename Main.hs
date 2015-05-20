{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Antenna.App
import Antenna.Sync
import Antenna.Types
import Control.Applicative                           ( (<$>) )
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString                        hiding ( any )
import Data.Maybe                                    ( mapMaybe )
import Data.Text                                     ( Text )
import Network.HTTP.Types
import Network.Wai 
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.HttpAuth
import System.Environment

corsPolicy _ = Just $ simpleCorsResourcePolicy{ corsMethods        = methods
                                              , corsRequestHeaders = headers }
  where
    methods = ["OPTIONS", "GET", "POST", "PUT", "PATCH", "DELETE"]
    headers = ["Authorization"]

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

data NodeType = Device ByteString ByteString | Virtual
    deriving (Show)

data Node = Node
    { nodeId :: Int 
    , node   :: NodeType 
    } deriving (Show)

data Store = Store
    { nodes :: [(Text, Node)] 
    } deriving (Show)

lookupTargets :: [(Text, Node)] -> [Text] -> [Int]
lookupTargets nodes = mapMaybe (fmap nodeId . flip lookup nodes) 

app :: Request -> WebM (AppState Store) Network.Wai.Response
app req = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    let us = userState as
    case pathInfo req of
        ["sync"] | "POST" == requestMethod req -> 
            case authenticate us req of
                Just nodeId -> do
                    body <- liftIO $ strictRequestBody req
                    case decode body of
                        Just Commit{..} -> do
                            let targetConsumers = lookupTargets (nodes us) targets
                            resp <- processSyncRequest nodeId targetConsumers log syncPoint
                            respondWith status200 resp
                        _ -> respondWith status400 (JsonError "BAD_REQUEST")
                Nothing -> 
                    respondWith status401 (JsonError "UNAUTHORIZED")
        ["ping"] -> return $ responseLBS status200 [] "Pong!"
        _ -> respondWith status404 (JsonError "NOT_FOUND")
  where
    respondWith status = return . responseLBS status [("Content-type", "application/json")] . encode 

authenticate :: Store -> Request -> Maybe Int
authenticate (Store nodes) req = 
    auth (join $ fmap extractBasicAuth $ lookup "Authorization" $ requestHeaders req)
  where
    auth Nothing = Nothing
    auth (Just (uname, pword)) = go nodes
      where
        go [] = Nothing
        go ((_, Node nid (Device u p)):xs) 
            | u == uname && p == pword = Just nid
            | otherwise = go xs

main :: IO ()
main = do
    port <- liftM read $ getEnv "PORT"    
    runWai port store app $ const [ cors corsPolicy ]
  where
    store = Store [ ("alice", Node 4 (Device "XX" "XX"))
                  , ("boris", Node 5 (Device "YY" "YY"))
                  , ("area-1", Node 100 Virtual)
                  ]

