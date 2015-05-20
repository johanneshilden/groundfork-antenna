{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Antenna.Types
  ( Action(..)
  , Command(..)
  , Commit(..)
  , Index(..)
  , Method(..)
  , Node(..)
  , Response(..)
  , SyncPoint(..)
  , Timestamp(..)
  , Notification(..)
  , SubscribeMsg(..)
  , insertCommitId
  , time
  ) where

import Control.Applicative
import Control.Monad                                 ( mzero )
import Data.Aeson
import Data.Function                                 ( on )
import Data.Int                                      ( Int64 )
import Data.IxSet                             hiding ( null )
import Data.Scientific                               ( coefficient )
import Data.Text                                     ( Text )
import Data.Typeable

data SubscribeMsg = SubscribeMsg
    { subscribeNode :: Int }

instance ToJSON SubscribeMsg where
    toJSON SubscribeMsg{..} = object 
        [ "node" .= subscribeNode ]
 
instance FromJSON SubscribeMsg where
    parseJSON (Object v) = fmap SubscribeMsg (v .: "node")
    parseJSON _ = mzero

data Notification = Notification
    { msgNode      :: Int
    , msgSyncpoint :: SyncPoint 
    }

instance ToJSON Notification where
    toJSON Notification{..} = object 
        [ "node"      .= msgNode
        , "syncPoint" .= msgSyncpoint
        ]
 
-- | Supported methods
data Method = POST | PUT | PATCH | DELETE
    deriving (Eq, Show)

instance FromJSON Method where
    parseJSON (String "POST")   = return POST
    parseJSON (String "PUT")    = return PUT
    parseJSON (String "PATCH")  = return PATCH
    parseJSON (String "DELETE") = return DELETE
    parseJSON _ = mzero

instance ToJSON Method where
    toJSON POST   = String "POST"
    toJSON PUT    = String "PUT"
    toJSON PATCH  = String "PATCH"
    toJSON DELETE = String "DELETE"

-- | A command operates on some application resource using the given method
--   and an optional request object. 
--
--   This technique for separation of execution of a command from its invoker 
--   is known in OOP contexts as the Command Pattern.
data Command = Command
    { method   :: Method
    , resource :: Text
    , payload  :: Maybe Value
    } deriving (Eq, Show, Typeable)

instance FromJSON Command where
    parseJSON (Object v) =
        Command <$> v .:  "method"
                <*> v .:  "resource"
                <*> v .:? "payload"
    parseJSON _ = mzero

instance ToJSON Command where
    toJSON Command{..} = object $
        [ "method"   .= method
        , "resource" .= resource
        ] ++ case "payload" .= payload of
              (_, Null) -> []
              x         -> [x]

newtype Timestamp = Timestamp Int64
    deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON)

newtype Node = Node Int
    deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON)

type CommitId   = Int
type BatchIndex = Int

data Index = Index 
    { commitId   :: CommitId
    -- ^ The id of the commit to which the indexed action belongs
    , batchIndex :: BatchIndex
    -- ^ A sequential id assigned to each action within a batch
    } deriving (Eq, Ord, Show, Typeable)

insertCommitId :: Index -> CommitId -> Index
insertCommitId (Index _ i) cid = Index cid i

instance FromJSON Index where
    parseJSON (Number n) = return $ Index 0 $ toInt n
      where toInt = fromIntegral . coefficient 
    parseJSON _ = mzero

data Action = Action
    { index     :: Index
    -- ^ The commit batch to which the action belongs and a batch-specific 
    --   sequential id assigned to each action.
    , timestamp :: Timestamp
    -- ^ A timestamp denoting when the action was created.
    , range     :: [Node]
    -- ^ A set containing the nodes that have executed this action.
    , up        :: Command
    -- ^ Command object which encapsulates the forward (redo) action.
    , down      :: Command
    -- ^ Command object which encapsulates the reverse (undo) action.
    } deriving (Eq, Show, Typeable)

instance Ord Action where
    compare = compare `on` tic
      where
        tic i = (timestamp i, index i)

instance FromJSON Action where
    parseJSON (Object v) =
        Action <$> v .: "index"
               <*> v .: "timestamp"
               <*> return []
               <*> v .: "up"
               <*> v .: "down"
    parseJSON _ = mzero

instance ToJSON Action where
    toJSON Action{..} = object
        [ "index"     .= batchIndex index
        , "commitId"  .= commitId index
        , "timestamp" .= timestamp
        , "range"     .= range
        , "up"        .= up
        , "down"      .= down
        ]

instance Indexable Action where
    empty = ixSet
        [ ixFun $ \u -> [timestamp u]
        , ixFun $ \u -> [Time $ timestamp u]
        , ixFun $ \u -> [index u]
        , ixFun range 
        ]
        
data SyncPoint = Time Timestamp | Saturated
    deriving (Eq, Show, Typeable)

time :: Int64 -> SyncPoint
time = Time . Timestamp

instance Ord SyncPoint where
    compare (Time a) (Time b)  = compare a b
    compare (Time _) Saturated = LT
    compare Saturated (Time _) = GT
    compare _ _                = EQ

instance FromJSON SyncPoint where
    parseJSON (String "*") = return Saturated
    parseJSON (Number   n) = return $ Time $ Timestamp $ fromIntegral $ coefficient n
    parseJSON _            = mzero

instance ToJSON SyncPoint where
    toJSON Saturated = String "*"
    toJSON (Time t)  = toJSON t

data Commit = Commit
    { targets   :: [Text]
    , log       :: [Action]
    , syncPoint :: SyncPoint
    } deriving (Show)

instance FromJSON Commit where
    parseJSON (Object v) =
        Commit <$> v .: "targets"
               <*> v .: "commit"
               <*> v .: "syncPoint"
    parseJSON _ = mzero

data Response = Response
    { respRewind     :: [Command]
    , respForward    :: [Command]
    , respSyncPoint  :: SyncPoint
--    , commitL        :: [Action]            -- temp
--    , syncPts        :: [(Int, SyncPoint)]  -- temp
    } deriving (Show)

instance ToJSON Response where
    toJSON Response{..} = object
        [ "reverse"   .= respRewind
        , "forward"   .= respForward
        , "syncPoint" .= respSyncPoint
--        , "log"       .= commitL
--        , "syncp"     .= syncPts
        ]