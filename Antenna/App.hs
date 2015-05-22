{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Antenna.App 
  ( AppState(..)
  , WebM
  , runWai
  , notify
  , lookupTargets
  , virtual
  ) where

import Antenna.Types
import Control.Applicative                           ( Applicative, (<$>), (<*>) )
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.IxSet                             hiding ( null )
import Data.Maybe                                    ( mapMaybe )
import Data.Text                                     ( Text )
import Network.Wai 
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets                            ( PendingConnection, forkPingThread, receiveDataMessage, defaultConnectionOptions )

import qualified Network.Wai                      as Wai
import qualified Network.WebSockets               as WS

type ConnectionId = Int

data AppState a = AppState
    { transLog     :: IxSet Action
    -- ^ Master transaction log
    , syncPoints   :: [(NodeId, SyncPoint)]
    -- ^ Sync points for known nodes
    , commitCount  :: Int
    -- ^ Counter holding next commit id
    , nodes        :: [(Text, Node)]
    -- ^ Node data
    , userState    :: a
    -- ^ Application-specific state
    , listeners    :: [(ConnectionId, (WS.Connection, Int))]
    -- ^ List of active WebSocket connections listening for log changes.
    , listenerId   :: Int
    }

virtual :: [(Text, Node)] -> [Int]
virtual = map (nodeId' . snd) . filter isVirtual 
  where
    isVirtual (_, Node _ t)
        | Virtual == t = True
        | otherwise   = False

lookupTargets :: [(Text, Node)] -> [Text] -> [Int]
lookupTargets nodes = mapMaybe (fmap nodeId' . flip lookup nodes) 

notify :: [(NodeId, SyncPoint)] -> WebM (AppState a) ()
notify xs = do
    var <- ask 
    liftIO $ readTVarIO var >>= forM_ xs . send 
  where
    send :: AppState a -> (NodeId, SyncPoint) -> IO ()
    send AppState{..} (NodeId node, sp) = 
        let msg = WS.Text $ encode $ Notification node sp
            subscribers = filter ((==) node . snd . snd) listeners
         in mapM_ (flip WS.sendDataMessage msg . fst . snd) subscribers

type WebM a = ReaderT (TVar a) IO 

wsApp :: TVar (AppState a) -> PendingConnection -> IO ()
wsApp tvar pending = do
    connection <- WS.acceptRequest pending
    as <- readTVarIO tvar 
    let connId = succ $ listenerId as
    atomically $ writeTVar tvar $ as{ listenerId = connId }
    forkPingThread connection 5
    let loop = do
            commandMsg <- receiveDataMessage connection
            case commandMsg of
              WS.Text txt -> 
                let n = decode txt >>= \(SubscribeMsg msg) -> lookup msg (nodes as)
                 in addListener connId n connection >> loop
              _ -> loop
    catch loop (close connId)
  where
    modifyListenersWith action = do
        as <- readTVarIO tvar 
        atomically $ writeTVar tvar as{ listeners = action $ listeners as }

    addListener :: ConnectionId -> Maybe Node -> WS.Connection -> IO ()
    addListener _ Nothing _ = return ()
    addListener connId (Just _node) conn = 
        modifyListenersWith $ (:) (connId, (conn, nodeId' _node))

    close :: ConnectionId -> WS.ConnectionException -> IO ()
    close connId _ = 
        modifyListenersWith $ filter ((/=) connId . fst) 

runWai :: Int -> a -> (Request -> WebM (AppState a) Network.Wai.Response) -> (TVar (AppState a) -> [Middleware]) -> IO ()
runWai port us app midware = do
    tvar <- newTVarIO initialState
    let waiApp = foldr ($) ((>>=) . flip runReaderT tvar . app) (midware tvar)
    run port $ websocketsOr defaultConnectionOptions (wsApp tvar) waiApp
  where
    initialState = AppState
        { transLog     = empty
        , syncPoints   = [
            (NodeId 4,   Saturated)
          , (NodeId 5,   Saturated)
          , (NodeId 100, Saturated)
          ]
        , commitCount  = 1
        , nodes        = [
            ("alice",  Node (NodeId 4) Device)
          , ("bob",    Node (NodeId 5) Device)
          , ("area-1", Node (NodeId 100) Virtual)
          ]
        , userState    = us
        , listeners    = []
        , listenerId   = 0
        }

