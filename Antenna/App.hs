{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Antenna.App 
  ( AppState(..)
  , WebM
  , runWai
  , notify
  ) where

import Antenna.Types
import Control.Applicative                           ( Applicative, (<$>), (<*>) )
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.IxSet                             hiding ( null )
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
    , syncPoints   :: [(Int, SyncPoint)]
    -- ^ Sync points for known nodes
    , commitCount  :: Int
    -- ^ Counter holding next commit id
    , virtualNodes :: [Int]
    -- ^ Nodes that do not represent any actual host device. Unlike ordinary 
    --   nodes, these nodes are automatically forwarded during a sync, if
    --   they appear as a target node. This makes them suitable as simpl 
    --   exchange points.
    , userState    :: a
    -- ^ Application-specific state.
    , listeners    :: [(ConnectionId, (WS.Connection, Int))]
    -- ^ List of active WebSocket connections listening for log changes.
    , listenerId   :: Int
    }

notify :: [(Int, SyncPoint)] -> WebM (AppState a) ()
notify xs = do
    var <- ask 
    liftIO $ do
        as <- readTVarIO var 
        mapM_ (send as) xs
  where
    send AppState{..} (node, sp) = 
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
                    case decode txt of
                      Nothing  -> loop
                      Just msg -> addListener connId msg connection >> loop
              _ -> loop
    catch loop (close connId)
  where
    modifyListenersWith action = do
        as <- readTVarIO tvar 
        atomically $ writeTVar tvar as{ listeners = action $ listeners as }

    addListener :: ConnectionId -> SubscribeMsg -> WS.Connection -> IO ()
    addListener connId msg conn = 
        modifyListenersWith $ (:) (connId, (conn, subscribeNode msg))

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
        , syncPoints   = [(4, Saturated), (5, Saturated)]
        , commitCount  = 1
        , virtualNodes = [100]            -- []
        , userState    = us
        , listeners    = []
        , listenerId   = 0
        }

