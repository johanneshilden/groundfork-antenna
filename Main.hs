{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Antenna.App
import Antenna.Routes
import Antenna.Types
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.ByteString                               ( ByteString )
import Data.Text                                     ( Text, unpack )
import Data.Text.Encoding                            ( encodeUtf8 )
import Network.HTTP.Types
import Network.Wai 
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.HttpAuth
import System.Environment
import Text.Read                                     ( readMaybe )

import qualified Data.HashMap.Strict              as HMS
import qualified Data.Text                        as T

corsPolicy _ = Just $ simpleCorsResourcePolicy{ corsMethods        = methods
                                              , corsRequestHeaders = headers }
  where
    methods = ["OPTIONS", "GET", "POST", "PUT", "PATCH", "DELETE"]
    headers = ["Authorization"]

data Store = Store
    { devices :: [(NodeId, (ByteString, ByteString))] 
    } deriving (Show)

data NodeTemplate = NodeTemplate
    { newName   :: Text
    , newType   :: NodeType
    , newDevice :: Maybe (ByteString, ByteString)
    } deriving (Show)

instance FromJSON NodeTemplate where
    parseJSON (Object v) =
        NodeTemplate <$> v .:  "name"
                     <*> v .:  "type"
                     <*> deviceInfo (HMS.lookup "device" v)
        where
          deviceInfo (Just (String str)) = 
            return $ case T.split (==':') str of
              [k,v] -> Just (encodeUtf8 k, encodeUtf8 v)
              _     -> Nothing
          deviceInfo _ = return Nothing
    parseJSON _ = mzero

app :: Request -> WebM (AppState Store) Network.Wai.Response
app req = 
    case pathInfo req of
        ["sync"] | "POST" == requestMethod req -> 
            ifAuthenticated $ runSyncRequest req 
        ["stack", page, size] -> 
            case (readMaybe (T.unpack page), readMaybe (T.unpack size)) of
                (Just p, Just s) -> ifAuthenticated $ const $ getStack p s
                _                -> respondWith status404 (JsonError "NOT_FOUND")
        ["reset"] | "POST" == requestMethod req -> 
            ifAuthenticated $ const resetStack
        ["nodes", node] ->
            case readMaybe (T.unpack node) of
              Just n -> ifAuthenticated $ const $ 
                case requestMethod req of
                  "DELETE" -> deleteNode n
                  "PUT" -> do
                        body <- liftIO $ strictRequestBody req
                        case decode body of
                          Just (name, xs) -> updateNode n name xs
              Nothing -> respondWith status404 (JsonError "NOT_FOUND")
        ["nodes"] -> 
            ifAuthenticated $ const
                $ case requestMethod req of
                    "GET" -> getNodes
                    "POST" -> do
                        body <- liftIO $ strictRequestBody req
                        case decode body of
                            Just NodeTemplate{..} -> do
                                _id <- freshNodeId 
                                when (newType == Device) $ insertDevice _id newDevice
                                insertNode newName (Node _id newType False [])
                            _ -> respondWith status400 (JsonError "BAD_REQUEST")
        ["ping"] -> return $ responseLBS status200 [] "Pong!"
        _ -> respondWith status404 (JsonError "NOT_FOUND")
  where
    insertDevice nid (Just dev) = do
        tvar <- ask
        liftIO $ atomically $ modifyTVar tvar $ \as -> 
            as{ userState = Store $ (nid, dev):devices (userState as) }
    insertDevice _ _ = return ()
    freshNodeId = do
        tvar <- ask
        as <- liftIO $ readTVarIO tvar
        let ids = nodeId' . snd <$> nodes as
        return $ NodeId $ succ $ maximum ids
    ifAuthenticated method = do
        tvar <- ask
        as <- liftIO $ readTVarIO tvar
        case authenticate (userState as) req of
          Just nodeId -> method nodeId
          Nothing -> respondWith status401 (JsonError "UNAUTHORIZED")
 
authenticate :: Store -> Request -> Maybe Int
authenticate (Store devices) req = 
    auth (join $ fmap extractBasicAuth $ lookup "Authorization" $ requestHeaders req)
  where
    auth Nothing = Nothing
    auth (Just (uname, pword)) = go devices
      where
        go [] = Nothing
        go ((NodeId nid, (u, p)):xs)
            | u == uname && p == pword = Just nid
            | otherwise = go xs

main :: IO ()
main = do
    port <- liftM read $ getEnv "PORT"    
    runWai port store app $ const [ cors corsPolicy ]
  where
    store = Store [ (NodeId 1, ("alice", "pwd"))
                  , (NodeId 2, ("bob",   "xxx"))
                  ]

