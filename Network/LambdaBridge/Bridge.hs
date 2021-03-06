{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, DataKinds, KindSignatures #-}

module Network.LambdaBridge.Bridge
        ( -- * Bridge
         Bridge(..)
        , Fragmentation(..)
        , Integrity(..)
        -- * Bridge Roots
        , connection
        , mVarBridge
        , handleBridge
        -- * Bridge Caps
        , echo
        -- * Bridge Lifters
        , debugBridge
        , serialize
        , unchecked
        , checked
        , dropping
        , noise
        , baud
        ) where

import Network.LambdaBridge.Logging

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Exception as Exc
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.Word
import Data.String
import System.Random
import Data.Time.Clock
import System.IO


debug = debugM "lambda-bridge.bridge"

data Fragmentation     = Framed                 -- ^ sequence of bytes that stay together
                       | Streamed               -- ^ can arived in fragements, or joined

data Integrity         = Trustworthy            -- ^ it will get there, can be trusted
                       | Checked                -- ^ if it gets there, it is good (CRC)
                       | Unchecked              -- ^ best effort, may be scrambled, lost, etc.

-- | A 'Bridge' is a bidirectional connection to a specific remote API.
-- There are many different types of Bridges in the lambda-bridge API.

data Bridge (fragmentation :: Fragmentation)
            (integrity     :: Integrity)
   = Bridge
        { toBridge 	:: BS.ByteString -> IO ()  -- ^ write to a bridge; may block; called many times.
	, fromBridge	:: IO BS.ByteString        -- ^ read from a bridge; may block, called many times.
        }

{-------------------------------------------------------------------
                Streamed                Framed

Unchecked       RS232                   SLIPed
Checked         (parity)                UDP
Trustworthy     TCP (sockets)           TCP (packets)

-------------------------------------------------------------------------

Layer           Datatype                        Example of Protocol

Transport       Bridge Framed Checked           UDP     (Datagram of bytes, checked by CRC)
Link            Bridge Framed Unchecked         SLIP    (Frame of bytes, can be garbled)
Physical        Bridge Fragmented Unchecked     RS232   (byte-wise transmission, can be unreliable)

--------------------------------------------------------------------------}


-- | for simulations and testing, provide two bridges, and connect them underneath.
connection :: IO (Bridge Framed Trustworthy,Bridge Framed Trustworthy)
connection = do
        v1 <- newEmptyMVar
        v2 <- newEmptyMVar
        b1 <- mVarBridge v1 v2
        b2 <- mVarBridge v2 v1
        return (b1,b2)

-- | This turns a input MVar and output MVar into a trustworthy bridge of frames.
-- Note that empty bytestrings are not supported (discarded by toBridge; ignored by fromBridge).

mVarBridge :: MVar ByteString -> MVar ByteString -> IO (Bridge Framed Trustworthy)
mVarBridge in_var out_var = do
        return $ Bridge
               { toBridge = \ bs ->
                        if BS.null bs
                        then return ()
                        else putMVar out_var bs
               , fromBridge =
                        let loop = do
                                bs <- takeMVar in_var
                                if BS.null bs
                                        then loop
                                        else return bs
                        in loop
               }

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

debugBridge :: String -> Bridge f i -> IO (Bridge f i)
debugBridge name bridge = do
	sendCounter <- newMVar 0
	recvCounter <- newMVar 0

	return $ Bridge
		{ toBridge = \ a -> do
			count <- takeMVar sendCounter
			putMVar sendCounter (succ count)
			debug $ name ++ ":toBridge<" ++ show count ++ "> (" ++ show a ++ ")"
			() <- toBridge bridge a
			debug $  name ++ ":toBridge<" ++ show count ++ "> success"
		, fromBridge = do
			count <- takeMVar recvCounter
			putMVar recvCounter (succ count)
			debug $  name ++ ":fromBridge<" ++ show count ++ ">"
			a <- fromBridge bridge `Exc.catch` \ (e :: SomeException) -> do { debug (show e) ; throw e }
			debug $  name ++ ":fromBridge<" ++ show count ++ "> (" ++ show a ++ ")"
			return a
		}


-- | cap a bridge with a simple echo. The ByteString function
-- is called on every packet or virtual packet.

echo :: (ByteString -> ByteString) -> Bridge framing integrity -> IO ()
echo f bridge = do
        v <- newEmptyMVar
        forkIO $ forever $ do
                bs <- fromBridge bridge
                toBridge bridge (f bs)
        return ()


-- | Downgrade a (framed) bridge to a streamed bridge.
serialize :: Bridge framing Trustworthy -> Bridge Streamed Trustworthy
serialize (Bridge to from) = Bridge to from

-- | Downgrade an unchecked bridge
unchecked :: Bridge framing integrity  -> Bridge framing Unchecked
unchecked  (Bridge to from) = Bridge to from

-- | Downgrade an checked bridge from a trustworthy bridge
checked :: Bridge framing Trustworthy  -> Bridge framing Checked
checked  (Bridge to from) = Bridge to from

-- | drop packets/chars. By convention, only the fromBridge side
-- has drops (concept: reception is when we detect errors).
-- 'connection' can be used to introduce bidirectional drops.

dropping :: Float -> Bridge framing Trustworthy -> IO (Bridge framing Unchecked)
dropping n bridge =
        return $ Bridge { toBridge = toBridge bridge
                        , fromBridge = do
                                let loop = do
                                        bs <- fromBridge bridge
                                        q :: Float <- randomIO
                                        if q < n then loop
                                                 else return bs
                                loop
                        }

-- | introduce noise. By convention, only the fromBridge side
-- has noise introduced (concept: reception is when we detect errors).
-- 'connection' can be used to introduce bidirectional noise.

noise :: Float -> Bridge Streamed integrity -> IO (Bridge Streamed Unchecked)
noise n bridge = do
        v_in :: MVar Word8 <- newEmptyMVar
        v_out :: MVar Word8 <- newEmptyMVar

        forkIO $ forever $ do
                bs <- fromBridge bridge
                sequence_ [ putMVar v_in b | b <- BS.unpack bs ]

        forkIO $ forever $ do
                r :: Float <- randomIO
                if r < n then do -- do something else
                                 q :: Int <- randomRIO (0,2)
                                 case q of
                                        -- drop the input
                                   0 -> do _ <- takeMVar v_in
                                           debug $ "dropping char"
                                           return ()
                                        -- add a char
                                   1 -> do w <- randomIO
                                           debug $ "adding char"
                                           putMVar v_out w
                                           return ()
                                        -- scramble the char
                                   2 -> do _ <- takeMVar v_in
                                           w <- randomIO
                                           debug $ "scramble char"
                                           putMVar v_out w
                                           return ()
                         else do w <- takeMVar v_in
                                 debug $ "id char"
                                 putMVar v_out w

        return $ Bridge { toBridge = toBridge bridge
                        , fromBridge = do
                                w <- takeMVar v_out
                                return $ BS.pack [w]
                        }


-- | Slow down the rx values, fixing the speed at (approximately) the given baud rate.
baud :: Int -> Bridge Streamed integrity -> IO (Bridge Streamed integrity)
baud baud_rate bridge = do
        let rate :: Float
            rate = 10 / fromIntegral baud_rate

        tm <- getCurrentTime
        tm_var <- newMVar tm

        v <- newEmptyMVar

        -- flatten/stream the RX.
        forkIO $ let loop [] = do
                        bs <- fromBridge bridge
                        loop (BS.unpack bs)
                     loop (c:cs) = do
                        putMVar v c
                        loop cs
                 in loop []

        return $ Bridge
               { toBridge = toBridge bridge
               , fromBridge = do
                       tm0 <- takeMVar tm_var
                       c <- takeMVar v
                       tm1 <- getCurrentTime
                       let diff :: Float = realToFrac $ tm1 `diffUTCTime` tm0
                       tm2 <- if (diff < rate)
                              then do threadDelay (floor (rate * 1000 * 1000))
                                      getCurrentTime
                              else do return tm1
                       let diff2 :: Float = realToFrac $ tm2 `diffUTCTime` tm0
                       let discount = min (realToFrac $ diff2 - rate) (realToFrac rate * 10)
                       putMVar tm_var (addUTCTime (-discount) tm2)
                       return (BS.pack [c])
                }

