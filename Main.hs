{-# LANGUAGE OverloadedStrings #-}
module Main where

import Antenna.App
import Antenna.Routes
import Antenna.Types
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.ByteString                        hiding ( any )
import Data.Text                                     ( Text )
import Network.HTTP.Types
import Network.Wai 
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.HttpAuth
import System.Environment

corsPolicy _ = Just $ simpleCorsResourcePolicy{ corsMethods        = methods
                                              , corsRequestHeaders = headers }
  where
    methods = ["OPTIONS", "GET", "POST", "PUT", "PATCH", "DELETE"]
    headers = ["Authorization"]

data Store = Store
    { devices :: [(NodeId, (ByteString, ByteString))] 
    } deriving (Show)

app :: Request -> WebM (AppState Store) Network.Wai.Response
app req = 
    case pathInfo req of
        ["sync"] | "POST" == requestMethod req -> 
            runIfAuthenticated $ runSyncRequest req 
        ["stack", page] -> getStack
            --runIfAuthenticated $ const getStack
        ["devices"] -> getDevices
            --runIfAuthenticated $ const getStack
        ["ping"] -> return $ responseLBS status200 [] "Pong!"
        _ -> respondWith status404 (JsonError "NOT_FOUND")
  where
    runIfAuthenticated method = do
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
    store = Store [ (NodeId 4, ("alice", "pwd"))
                  , (NodeId 5, ("bob", "xxx"))
                  ]

