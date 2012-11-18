{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.SLIP
import Network.LambdaBridge.CRC
import Network.LambdaBridge.Bus hiding (debug)
import Network.LambdaBridge.Logging
import Network.LambdaBridge.Board hiding (debug)

import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Monad
import Data.Monoid
import Data.String
import Data.Char
import System.IO
import Data.Time.Clock
import System.Random

debug = debugM "lambda-bridge.main"


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
        v2 <- newEmptyMVar

        brd0 <- stateMachineToBus rxInitialState $ rxStateMachine v1
        brd2 <- stateMachineToBus txInitialState $ txStateMachine v2

        forkIO $ forever $ do
                v <- takeMVar v1
                debug $ "got " ++ show v
                putMVar v2 (v)
                debug $ "sent " ++ show v

--        forkIO $ forever  $ do
--                sequence [ putMVar v2 n | n <- [100..200]]


        let brd = virtualBus
                [ (0, brd0)
                , (2, brd2)
                ]

        return brd



test_baud rate = do
        -- our backbone, level 0
        (lhs0,rhs0) <- connection

        lhs1 <- baud rate  (serialize lhs0)
        rhs1 <- baud rate  (serialize rhs0)

        echo id rhs1    -- cap the far end

        tm0 <- getCurrentTime

        count <- newMVar 0

        forkIO $ forever $ do
                toBridge lhs1 $ BS.pack [33..100]

        let pp n | isAscii n || n == '\n' = [n]
                 | otherwise = "."

        forever $ do
                bs <- fromBridge lhs1
                n <- takeMVar count
                let n' = n + BS.length bs
                putMVar count n'
                tm1 <- getCurrentTime
                let diff :: Float = realToFrac $ tm1 `diffUTCTime` tm0
                if (n `mod` 1000 == 0) then
                        putStrLn $ show n' ++ " bytes in " ++ show diff ++ " seconds, rate = "
                                ++ show (floor (1 / (diff / fromIntegral n')) )
                    else return ()

main2 "115200" = test_baud 115200
main2 "9600"   = test_baud 9600

main = do
        setStdGen (mkStdGen 1000)
--        init_logging
        hSetBuffering stdout LineBuffering
        hSetBuffering stdout LineBuffering

        sim_brd <- simBoard

        -- our backbone, level 0
        (lhs0,rhs0) <- connection

        -- introduce delays
        let rate = 9600
        lhs1 <- baud rate  (serialize lhs0)
        rhs1 <- baud rate  (serialize rhs0)

        -- introduce errors

        lhs2 <- noise 0.0001 (serialize lhs1)
                                        ; rhs2 <- noise 0.0001 (serialize rhs1)


        -- level 1                              simulator stack
        lhs3 <- slipProtocol lhs2       ;   rhs3 <- slipProtocol rhs2

        -- level 2

        lhs4 <- crc16Protocol lhs3      ;   rhs4 <- crc16Protocol rhs3

        -- level 3

        lhs_brd <- connectToBus 0.1 lhs4
                                        ; interpBus rhs4 sim_brd

        -- with the bridge set up, we can do testing


        p0 <- busWritePort lhs_brd 0
        p2 <- busReadPort lhs_brd 2

        let text :: [String]
            text = [ show n ++ " + " ++ show n ++ " = " ++ show (n+n) ++ "\n"
                   | n <- [0..100]
                   ]

        forkIO $ do
                sequence_ [ writePort p0 $ BS.pack $ map (fromIntegral . ord) txt
                          | txt <- text
                          ]

        let pp n | isAscii n = [n] -- ['<',n,'>']
                 | otherwise = "."

        forever $ do
                bs <- readPort p2
                putStr $ concatMap pp $ map (chr . fromIntegral) $ BS.unpack $ bs
                hFlush stdout


        return ()