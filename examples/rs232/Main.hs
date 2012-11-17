module Main where

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.SLIP
import Network.LambdaBridge.CRC
import Network.LambdaBridge.Bus
import Network.LambdaBridge.Bus.Simulator

import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Monad
import Data.Monoid
import Data.String
import System.IO

io b = do
        forkIO $ forever $ do
                bs <- fromBridge b
                print bs

        let loop 1000000 = return ()
            loop n = do
                toBridge b $ fromString ("packet: " ++ show n)
                loop (n+1)

        loop 0

simBoard :: IO Board
simBoard = do
        v1 <- newEmptyMVar

        brd0 <- stateMachineToBus rxInitialState $ rxStateMachine v1
        brd2 <- stateMachineToBus txInitialState $ txStateMachine v1

        let brd = virtualBus
                [ (0, brd0)
                , (2, brd2)
                ]

        return brd


main = do
        hSetBuffering stdout LineBuffering
        hSetBuffering stdout LineBuffering

        sim_brd <- simBoard

        -- our backbone, level 0
        (lhs0,rhs0) <- connection

{-
        -- level 1                              simulator stack
        lhs1 <- slipProtocol lhs0       ;   rhs1 <- slipProtocol rhs0

        -- level 2

        lhs2 <- crc16Protocol lhs1      ;   rhs2 <- crc16Protocol rhs1

        -- level 3
-}

{-
        lhs_brd <- connectToBoard 1.0 (checked lhs0)
                                        ; interpBus (checked rhs0) sim_brd

-}
        p0 <- busWritePort sim_brd 0
        p2 <- busReadPort sim_brd 2

        forkIO $ do
                sequence_ [ writePort p0 (BS.pack [n])
                          | n <- take 10000 $ cycle [0..255]
                          ]

        forever $ do
                bs <- readPort p2
                print bs


{-
        echo id b2 -- (\ bs -> fromString ("<" ++ show bs ++ ">")) b2

        b3 <- noise 0.01 b1
        b4 <- buffer 1 b3
        b5 <- slipProtocol b4
        b6 <- crc16Protocol b5

        io b6
-}
        return ()