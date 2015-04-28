{-# LANGUAGE OverloadedStrings #-}
module Main where

import Antenna.App
import Antenna.Command
import Antenna.Sync
import Data.Aeson
import Data.IxSet                             hiding ( null )
import Data.Text.Lazy                                ( Text, fromStrict )
import Web.Scotty.Trans

app :: ScottyT Text WebM ()
app = 
    -- Sync request
    post "/" processSyncRequest

main :: IO ()
main = run app

-------------------------------------------------------------------------------

cmd = Command POST "X" (Just Null)

testData =
    foldr insert empty
        [ Action (Index 1 1) (Timestamp 1) [Node 1, Node 2]          cmd cmd
        , Action (Index 2 1) (Timestamp 2) [Node 1]                  cmd cmd
        , Action (Index 3 1) (Timestamp 1) []                        cmd cmd
        , Action (Index 1 2) (Timestamp 2) [Node 2, Node 1, Node 3]  cmd cmd
        , Action (Index 2 2) (Timestamp 1) [Node 3, Node 1]          cmd cmd
        ]

