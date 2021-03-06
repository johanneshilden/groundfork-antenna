{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Antenna.Sync 
  ( processSyncRequest
  , (|>)
  ) where

import Antenna.App
import Antenna.Types
import Control.Applicative                           ( Applicative, (<$>), (<*>) )
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson
import Data.Function ( on )
import Data.IxSet                             hiding ( null )
import Data.List                                     ( sort, sortBy, partition, intersect, nub )
import Data.List.Utils                               ( addToAL )
import Data.Monoid                                   ( (<>) )
import Data.Maybe                                    ( fromMaybe, fromJust, catMaybes, mapMaybe )
import Data.Text.Lazy                                ( Text, fromStrict )

import qualified Data.HashMap.Strict              as Map
import qualified Data.Text                        as TS
import qualified Data.Text.Lazy                   as Text
import qualified Text.Show.Text                   as Text

-- | Forward pipe operator
{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) a f = f a

infixl 3 |>

-- | Find all nodes for which the sync point changed
changedNodes :: [(NodeId, SyncPoint)] -> [(NodeId, SyncPoint)] -> [(NodeId, SyncPoint)]
changedNodes xs = mapMaybe f 
  where 
    f (a, b) = 
        let cond pred = if pred == b then Nothing 
                                    else Just (a, b) 
         in case lookup a xs of
              Nothing -> cond (time 0)
              Just b' -> cond b'

processSyncRequest :: Node -> [Int] -> [Action] -> SyncPoint -> WebM (AppState a) Response 
processSyncRequest sourceNode targets log syncPoint = do
    var <- ask 
    as <- liftIO $ readTVarIO var 
    let source = nodeId' sourceNode
        targets' = candidates sourceNode `intersect` targets  
        (as', r) = process source targets targets' log syncPoint as
    liftIO $ atomically $ writeTVar var as'
    notify $ filter ((/=) (NodeId source) . fst) $ (changedNodes `on`) syncPoints as as'
    return r

process :: Int -> [Int] -> [Int] -> [Action] -> SyncPoint -> AppState a -> (AppState a, Response)
process source targets candidateTargets log syncPoint AppState{..} = 

    -- Find most recent sync point stored for this source node 
    -- and compare it against the value provided in the request

    let savedSp = fromMaybe (Time $ Timestamp 0) (lookup (NodeId source) syncPoints')

        (ts, isAhead) = if syncPoint < savedSp
                            then (syncPoint, True)
                            else (savedSp, False)

        -- Substitute placeholder templated references and insert commited
        -- actions into transaction log

        transLog' = instantiate . annotate commitCount <$> log 
                        |> foldr insert transLog 
        
        -- Partition the selection based on whether an item's range contains
        -- the source or any of the target nodes

        (included, excluded) = transLog' @>= ts 
                    |> toList 
                    |> partition inRange
                    |> fmap sort            -- sort the excluded items

        sp = if null excluded then Saturated else Time $ timestamp $ head excluded

        in ( AppState 
                { transLog     = foldr addNodes transLog' included
                , syncPoints   = addToAL syncPoints' (NodeId source) sp
                , commitCount  = succ commitCount 
                , nodes        = nodes
                , userState    = userState
                , listeners    = listeners
                , listenerId   = listenerId
                }
            , Response
                { respRewind = 
                    if isAhead then []
                               else down <$> reverseCmds (transLog @>= ts @= NodeId source)
                , respForward   = up <$> sort included
                , respSyncPoint = sp 
--                , commitL = toList $ foldr addNodes transLog' included       -- @todo: remove
--                , syncPts = addToAL syncPoints' source sp }                  -- @todo: remove
            } )
  where

    -- Update sync points for all nodes to the least recent (min) of the current
    -- value and the timestamp of the first item in the commit log
    syncPoints' = 
        case log of
          []    -> syncPoints
          (a:_) -> fmap (min . Time $ timestamp a) <$> syncPoints

    -- Uploaded actions are annotated with the commit id
    annotate cid item@Action{..} =
        item{ index = insertCommitId index cid
            , range = [NodeId source] } 

    -- Predicate to single out items whose range contains a node which is 
    -- either the source or in the list of target nodes
    --
    -- (Only consider target nodes included in the node's candidate list.)
    inRange item@Action{..} = not $ null $ intersect range $ NodeId <$> (source:candidateTargets) 

    -- Return the set as a list, sorted by timestamp in descending order
    reverseCmds = sortBy (flip compare) . toList 

    -- Insert source node and "virtual" target nodes into the range of forwarded actions
    addNodes item@Action{..} = 
        let nodes' = NodeId <$> source : intersect targets (virtual nodes)
         in updateIx index item{ range = nub $ nodes' ++ range } 

instantiate :: Action -> Action
instantiate item@Action{ index = Index cid _, .. } = 
    item{ up = t up, down = t down }
  where

    t Command{..} = Command 
        { method   = method
        , resource = replace resource 
        -- Substitute for placeholder references in the request body
        , payload  = updObj <$> payload 
        }

    updObj (Object o) = Object $ Map.mapWithKey deep o 
    updObj o = o

    deep "href" (String val) = String $ replace val
    deep _ o = case o of
                 Object _ -> updObj o
                 _        -> o

    replace txt = TS.splitOn "||" txt |> zipWith (curry go) [1 .. ] 
                                      |> TS.concat 
    go (i, p) | odd i      = p
              | otherwise  = 
                 case TS.splitOn "/" p of
                   [_, i] | "-" `TS.isInfixOf` i -> p
                   [r, i] -> r <> "/id_" <> Text.show cid <> "-" <> i 
                   _      -> p

