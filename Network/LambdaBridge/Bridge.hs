{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, DataKinds, KindSignatures #-}

module Network.LambdaBridge.Bridge
        ( Bridge(..)
        , Fragmentation(..)
        , Integrity(..)
        , Delivery(..)
        , debugBridge
        , connection
        , echo
        , serialize
        , unchecked
        , checked
        , noise
        , buffer
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

debug = debugM "lambda-bridge.bridge"

data Fragmentation     = Framed                 -- ^ sequence of bytes that stay together
                       | Streamed               -- ^ can arived in fragements, or joined

data Integrity         = Trustworthy
                       | Checked                -- ^ CRC'd
                       | Unchecked              -- ^ raw bytes

data Delivery          = Reliable               -- ^ will alway get there
                       | Unreliable             -- ^ may not get there

-- | A 'Bridge' is a bidirectional connection to a specific remote API.
-- There are many different types of Bridges in the lambda-bridge API.

data Bridge (fragmentation :: Fragmentation)
            (integrity     :: Integrity)
   = Bridge
        { toBridge 	:: BS.ByteString -> IO ()  -- ^ write to a bridge; may block; called many times.
	, fromBridge	:: IO BS.ByteString        -- ^ read from a bridge; may block, called many times.
        }


{-
Trustworthy     -- it will get there, can be trusted
Checked         -- if it gets there, it is good
Unchecked       -- best effort, may be scrambled, lost, etc.
-}

{-------------------------------------------------------------------
                Framed                                  Fragmented

                Checked         UnChecked               Checked         UnChecked

Reliable        TCP             (*3)                    Socket(TCP)     (*1)

UnReliable      UDP             SLIP                    (*2)            RS232

Can you be Fragmented, Checked, but UnReliable?
Can you be Fragmented, UnChecked, but Reliable?
When Fragmented, the concepts of Reliable and Checked collapse into one??

(*1) = every char gets through, some are scrambled.
(*2) = every char that gets through was sent in that order.
(*3) = every packet get as there, may be scrambled


-------------------------------------------------------------------------

Layer           Datatype                        Example of Protocol

Transport       Bridge Framed Checked           UDP     (Datagram of bytes, checked by CRC)
Link            Bridge Framed Unchecked         SLIP    (Frame of bytes, can be garbled)
Physical        Bridge Fragmented Unchecked     RS232   (byte-wise transmission, can be unreliable)

--------------------------------------------------------------------------}

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



-- | for simulations and testing, provide two bridges, and connect them underneath.
connection :: IO (Bridge framing Trustworthy,Bridge framing Trustworthy)
connection = do
        v1 <- newEmptyMVar
        v2 <- newEmptyMVar
        return ( Bridge { toBridge = putMVar v2
                        , fromBridge = takeMVar v1
                        }
               , Bridge { toBridge = putMVar v1
                        , fromBridge = takeMVar v2
                        }
               )

-- | cap a bridge with a simple echo.
echo :: (ByteString -> ByteString) -> Bridge framing integrity -> IO ()
echo f bridge = do
        v <- newEmptyMVar
        forkIO $ forever $ do
                bs <- fromBridge bridge
                toBridge bridge (f bs)
        return ()


-- | Downgrade a framed bridge to a streamed bridge.
serialize :: Bridge Framed Trustworthy -> Bridge Streamed Trustworthy
serialize (Bridge to from) = Bridge to from

-- | Downgrade an unchecked bridge
unchecked :: Bridge framing integrity  -> Bridge framing Unchecked
unchecked  (Bridge to from) = Bridge to from

-- | Downgrade an checked bridge from a trustworthy bridge
checked :: Bridge framing Trustworthy  -> Bridge framing Checked
checked  (Bridge to from) = Bridge to from


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
                                           return ()
                                        -- add a char
                                   1 -> do w <- randomIO
                                           putMVar v_out w
                                           return ()
                                        -- scramble the char
                                   2 -> do _ <- takeMVar v_in
                                           w <- randomIO
                                           putMVar v_out w
                                           return ()
                         else do w <- takeMVar v_in
                                 putMVar v_out w

        return $ Bridge { toBridge = toBridge bridge
                        , fromBridge = do
                                w <- takeMVar v_out
                                return $ BS.pack [w]
                        }


-- Wait this number of (sub)-seconds for rx values.
buffer :: Float -> Bridge Streamed integrity -> IO (Bridge Streamed integrity)
buffer n bridge = return bridge        -- TODO

{-

-- To be added later

-- |  ''Realistic'' is the configuration for ''realisticBridge''.
data Realistic a = Realistic
	{ loseU 	:: Float	-- ^ lose an 'a'
	, dupU 		:: Float	-- ^ dup an 'a'
	, execptionU 	:: Float	-- ^ throw exception instead
	, pauseU	:: Float	-- ^ what is the pause between things
	, mangleU 	:: Float	-- ^ mangle an 'a'
	, mangler       :: Float -> a -> a -- ^ how to mangle, based on a number between 0 and 1
	}

-- | default instance of 'realistic', which is completely reliable.
instance Default (Realistic a) where
	def = Realistic 0 0 0 0 0 (\ g a -> a)


{-
connectBridges :: (Show msg) => Bridge msg -> Realistic msg -> Realistic msg -> Bridge msg -> IO ()
connectBridges lhs lhsOut rhsOut rhs = do
	let you :: Float -> IO Bool
	    you f = do
		r <- randomIO
		return $ f > r

	let optMangle f mangle a = do
		b <- you f
		if b then do
		        r <- randomIO
			return $ mangle r a
		     else return a


        let unrely :: MVar UTCTime -> Realistic msg -> msg -> (msg -> IO ()) -> IO ()
            unrely tmVar opts a k = do
		tm0 <- takeMVar tmVar	-- old char time
		tm1 <- getCurrentTime	-- current time
		let pause = pauseU opts - realToFrac (tm1 `diffUTCTime` tm0)
		if pause <= 0 then return () else do
		   threadDelay (floor (pause * 1000 * 1000))
		   return ()
		b <- you (loseU opts)
		if b then return () else do -- ignore if you "lose" the message.
		  a <- optMangle (mangleU opts) (mangler opts) a
		  b <- you (dupU opts)
		  if b then do		   -- send twice, please
		  	k a
		  	k a
		       else do
	                k a
		tm <- getCurrentTime
		putMVar tmVar tm
		return ()

	tm <- getCurrentTime
	tmVar1 <- newMVar tm
	tmVar2 <- newMVar tm

        forkIO $ forever $ do
                msg <- fromBridge lhs
                unrely tmVar1 lhsOut msg $ toBridge rhs
        forever $ do
                msg <- fromBridge rhs
                unrely tmVar2 rhsOut msg $ toBridge lhs
-}
-}