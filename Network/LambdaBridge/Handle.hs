{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, DataKinds, KindSignatures #-}
-- | This module provide the ability to build a server.

-- This can be used for building simulators of hardware board,
-- debugging, or pass-through components.

module Network.LambdaBridge.Handle (handleBridge) where


import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Logging

import System.IO
import qualified Data.ByteString as BS

debug = debugM "lambda-bridge.handle"

-- | This turns a input Handle and output Handle into a Bridge.

handleBridge :: Handle -> Handle -> IO (Bridge Streamed Trustworthy)
handleBridge in_hd out_hd = do
        hSetBuffering out_hd NoBuffering            -- for now
        hSetBuffering in_hd NoBuffering            -- for now

        return $ Bridge
               { toBridge = BS.hPut out_hd
               , fromBridge = do
                       bs <- BS.hGet in_hd 1
                       if BS.length bs == 0 then fail "socket closed"
                                            else return bs
               }