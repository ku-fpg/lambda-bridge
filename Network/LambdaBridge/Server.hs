-- | This module provide the ability to build a server.

-- This can be used for building simulators of hardware board,
-- debugging, or pass-through components.

module Network.LambdaBridge.Server (serverBridge) where


import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Logging

import System.IO
import Data.Char
import System.Directory
import Control.Exception
import Control.Monad
import Data.Char
import System.IO.Unsafe
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS
import Control.Concurrent

import Network
--import Network.Socket hiding (send, sendTo, recv, recvFrom)
--import Network.Socket.ByteString

debug = debugM "lambda-bridge.server"

-- | We intentually allow any style of bridge here, because
-- sometimes we use the server for testing network stacks.
--
-- In typical useage, Lambda Bridge does not use (software) servers
-- except for testing, or perhaps forwarding to real hardware.
--
serverBridge :: PortID -> IO (Bridge framing integrity)
serverBridge portId = error ""
{-
        finally (do sock <- listenOn portId
                    forever $ do
                       debug $ "accepting socket " ++ show sock
                       acc@(hd,_host,_port) <- accept sock
                       debug $ "accepted " ++ show acc
                       server hd)
                (case portId of
                   UnixSocket name -> removeFile name
                   _ -> return ())
  where
    server hd = do
        hSetBuffering hd NoBuffering            -- for now
        forkIO $ forever $ do
                bs <- BS.hGet hd 1
                if BS.length bs == 0 then fail "socket closed"
                                     else toBridge bridge bs

        forever $ do
                bs <- fromBridge bridge
                BS.hPut hd bs
-}
