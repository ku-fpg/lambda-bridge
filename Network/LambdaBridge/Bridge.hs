{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, DataKinds, KindSignatures #-}

module Network.LambdaBridge.Bridge where

import Network.LambdaBridge.Logging

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Exception as Exc
import Control.Concurrent.MVar

debug = debugM "lambda-bridge.bridge"

data Fragmentation     = Framed                 -- ^ sequence of bytes that stay together
                       | Streamed               -- ^ can arived in fragements, or joined

-- Later, add concept of reliableness, which implies checked
data Integrity         = Checked                -- ^ CRC'd
                       | UnChecked              -- ^ raw bytes

data Delivery          = Reliable               -- ^ will alway get there
                       | UnReliable             -- ^ may not get there

-- | A 'Bridge' is a bidirectional connection to a specific remote API.
-- There are many different types of Bridges in the lambda-bridge API.

-- data Property           = Framed

data Bridge (fragmentation :: Fragmentation)
            (integrity     :: Integrity)
            (delivery      :: Delivery)
   = Bridge
        { toBridge 	:: BS.ByteString -> IO ()  -- ^ write to a bridge; may block; called many times.
	, fromBridge	:: IO BS.ByteString        -- ^ read from a bridge; may block, called many times.
        }

{-------------------------------------------------------------------
                Framed                                  Fragmented

                Checked         UnChecked               Checked         UnChecked

Reliable        TCP             SLIP                    Socket(TCP)     (*1)

UnReliable      UDP             SLIP                    (*2)            RS232

Can you be Fragmented, Checked, but UnReliable?
Can you be Fragmented, UnChecked, but Reliable?
When Fragmented, the concepts of Reliable and Checked collapse into one??

(*1) = every char gets through, some are scrambled.
(*2) = every char that gets through was sent in that order.



-------------------------------------------------------------------------

Layer           Datatype                        Example of Protocol

Transport       Bridge Framed Checked UnReliable           UDP     (Datagram of bytes, checked by CRC)
Link            Bridge Framed Unchecked  UnReliable       SLIP    (Frame of bytes, can be garbled)
Physical        Bridge Fragmented Unchecked UnReliable    RS232   (byte-wise transmission, can be unreliable)

--------------------------------------------------------------------------}

debugBridge :: String -> Bridge f i r -> IO (Bridge f i r)
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