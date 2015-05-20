{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Antenna.App
import Antenna.Sync
import Antenna.Types
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Maybe                                    ( mapMaybe )
import Data.Text                                     ( Text )
import Network.HTTP.Types
import Network.Wai 
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import System.Environment

corsPolicy _ = Just $ simpleCorsResourcePolicy{ corsMethods = methods }
  where
    methods = ["OPTIONS", "GET", "POST", "PUT", "PATCH", "DELETE"]

data OkResponse = JsonOk (Maybe Value)

instance ToJSON OkResponse where
    toJSON (JsonOk mb) = object $
        [ ("status", "success")
        , ("message", "OK") 
        ] ++ case mb of
              Nothing -> []
              Just b  -> [("body", b)]

data ErrorResponse = JsonError Text

instance ToJSON ErrorResponse where
    toJSON (JsonError code) = object 
        [ ("status", "error") 
        , ("error", String code) ]

data Store = Store
    { nodes :: [(Text, Int)] }

lookupTargets :: [(Text, Int)] -> [Text] -> [Int]
lookupTargets nodes = mapMaybe (`lookup` nodes) 

app :: Request -> WebM (AppState Store) Network.Wai.Response
app req = do
    tvar <- ask
    as <- liftIO $ readTVarIO tvar
    let us = userState as
    case pathInfo req of
        ["sync"] | "POST" == requestMethod req -> do
            body <- liftIO $ strictRequestBody req
            case decode body of
                Just Commit{..} -> do
                    let targetNodes = lookupTargets (nodes us) targets
                    r <- processSyncRequest 4 targetNodes log syncPoint
                    return $ responseLBS status200 [("Content-type", "application/json")] $ encode r
                _ -> return $ responseLBS status400 [("Content-type", "application/json")] $ encode (JsonError "BAD_REQUEST")
        ["ping"] -> return $ responseLBS status200 [] "Pong!"
        _ -> return $ responseLBS status404 [("Content-type", "application/json")] $ encode (JsonError "NOT_FOUND")

main :: IO ()
main = do
    port <- liftM read $ getEnv "PORT"    
    runWai port store app $ const [ cors corsPolicy ]
  where
    store = Store [ ("alice", 4)
                  , ("boris", 5)
                  , ("area-1", 100)
                  ]

