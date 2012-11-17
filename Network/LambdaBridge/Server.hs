{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, DataKinds, KindSignatures #-}

-- | This module provide the ability to build a server.

-- This can be used for building simulators of hardware board,
-- debugging, or pass-through components.

module Network.LambdaBridge.Server (serverBridge) where


import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Logging
import Network.LambdaBridge.Handle

import Network

debug = debugM "lambda-bridge.server"

-- | We intentually allow any style of bridge here, because
-- sometimes we use the server for testing network stacks.
--
-- In typical useage, Lambda Bridge does not use (software) servers
-- except for testing, or perhaps forwarding to real hardware.
--
serverBridge :: PortID -> IO (Bridge Streamed Trustworthy)
serverBridge portId =
        do sock <- listenOn portId
           debug $ "accepting socket " ++ show sock
           acc@(hd,_host,_port) <- accept sock
           debug $ "accepted " ++ show acc
           handleBridge hd hd

