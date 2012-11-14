module Main where

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.SLIP
import Network.LambdaBridge.CRC

import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Monad
import Data.String

io b = do
        forkIO $ forever $ do
                bs <- fromBridge b
                print bs

        let loop 1000000 = return ()
            loop n = do
                toBridge b $ fromString ("packet: " ++ show n)
                loop (n+1)

        loop 0

main = do
        (b1,b2) <- connection

        echo id b2 -- (\ bs -> fromString ("<" ++ show bs ++ ">")) b2

        b3 <- noise 0.01 b1
        b4 <- buffer 1 b3
        b5 <- slipProtocol b4
        b6 <- crc16Protocol b5

        io b6
