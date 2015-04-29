{-# LANGUAGE OverloadedStrings #-}
module Main where

import Antenna.App
import Antenna.Command
import Antenna.Sync
import Control.Monad.Reader
import Data.Aeson
import Data.IxSet                             hiding ( null )
import Data.Text.Lazy                                ( Text, fromStrict )
import Network.HTTP.Types
import Network.Wai 
import Network.Wai.Handler.Warp

import qualified Data.ByteString.Lazy             as BL

xx :: Request -> WebM Network.Wai.Response
xx req = do
    -- todo : match on route
    body <- liftIO $ requestBody req
    case decode $ BL.fromStrict body of
      Nothing -> undefined       -- @todo
      Just o  -> do
        r <- processSyncRequest o
        return $ responseLBS status200 [("Content-type", "application/json")] $ encode r

main :: IO ()
main = runWai xx

-------------------------------------------------------------------------------

cmd = Command Antenna.Command.POST "X" (Just Null)

testData =
    foldr insert empty
        [ Action (Index 1 1) (Timestamp 1) [Node 1, Node 2]          cmd cmd
        , Action (Index 2 1) (Timestamp 2) [Node 1]                  cmd cmd
        , Action (Index 3 1) (Timestamp 1) []                        cmd cmd
        , Action (Index 1 2) (Timestamp 2) [Node 2, Node 1, Node 3]  cmd cmd
        , Action (Index 2 2) (Timestamp 1) [Node 3, Node 1]          cmd cmd
        ]

