{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Antenna.App 
  ( AppState(..)
  , WebM
  , runWai
  ) where

import Antenna.Command
import Control.Applicative                           ( Applicative, (<$>), (<*>) )
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Data.IxSet                             hiding ( null )
import Data.Text.Lazy                                ( Text )
import Network.HTTP.Types
import Network.Wai 
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors

import qualified Data.Text.Lazy                   as Text

data AppState = AppState
    { transLog     :: IxSet Action
    -- ^ Master transaction log
    , syncPoints   :: [(Int, SyncPoint)]
    -- ^ Sync points for known nodes
    , commitCount  :: Int
    -- ^ Counter holding next commit id
    , virtualNodes :: [Int]
    -- ^ Nodes that do not represent any actual host device. Unlike ordinary 
    --   nodes, these nodes are automatically forwarded during a sync in which 
    --   they appear as a target node. This makes them suitable as exchange 
    --   points.
    }

instance Default AppState where
    def = AppState
        { transLog     = empty
        , syncPoints   = []
        , commitCount  = 1
        , virtualNodes = [100]            -- []
        }

newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

runWai :: (Request -> WebM Network.Wai.Response) -> IO ()
runWai router = do
    sync <- newTVarIO def
    run 3333 $ simpleCors $ 
        (>>=) . flip runReaderT sync . runWebM . router

