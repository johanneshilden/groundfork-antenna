{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Antenna.App 
  ( AppState(..)
  , WebM
  , run
  ) where

import Antenna.Command
import Control.Applicative                           ( Applicative, (<$>), (<*>) )
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Data.IxSet                             hiding ( null )
import Data.Text.Lazy                                ( Text )
import Network.Wai.Middleware.Cors
import Web.Scotty.Trans

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
    --   nodes, these nodes are automatically moved forward when sync'd 
    --   against. This makes them suitable as exchange points.
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

run :: ScottyT Text WebM () -> IO ()
run app = do
    sync <- newTVarIO def

    let runM m = runReaderT (runWebM m) sync
        runActionToIO = runM

    scottyT 3333 runM runActionToIO $ do
        middleware simpleCors
        app

