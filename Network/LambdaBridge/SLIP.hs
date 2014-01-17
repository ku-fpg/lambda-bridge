{-# LANGUAGE DataKinds, TypeFamilies #-}


{- | Support for providing a Frame-based API on top of an bytestream, using SLIP.

   * <http://en.wikipedia.org/wiki/Serial_Line_Internet_Protocol>

   * <http://tools.ietf.org/html/rfc1055>


  The format for SLIP is simple: optional end tag, payload with stuffing, end tag.

> <-END-><- PAYLOAD with byte stuffing                  -><-END->
> +-----+-------------------------------------------------+-----+
> | 192 |  payload { 192 -> [219,220], 219 -> [219,221] } | 192 |
> +-----+-------------------------------------------------+-----+

-}

module Network.LambdaBridge.SLIP
	( slipProtocol
	, SLIP_Integrity
	) where


import Data.Word
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

import Network.LambdaBridge.Logging (debugM)
import Network.LambdaBridge.Bridge


type family SLIP_Integrity (a :: Integrity) :: Integrity
type instance SLIP_Integrity Trustworthy = Trustworthy
type instance SLIP_Integrity Checked     = Unchecked
type instance SLIP_Integrity Unchecked   = Unchecked

slipProtocol :: Bridge framing integrity
             -> IO (Bridge Framed
                           (SLIP_Integrity integrity)
                   )
slipProtocol bytes_bridge = do
        let debug = debugM "lambda-bridge.slip"

        let end     = 192
            esc     = 219
            esc_end = 220
            esc_esc = 221

        let bytestuff :: Word8 -> [Word8]
            bytestuff c | c == end  = [esc, esc_end]
                        | c == esc  = [esc, esc_esc]
                        | otherwise = [c]

        let sendFrame = \ bs -> do
                let packet = [end] ++ concatMap bytestuff (BS.unpack bs) ++ [end]
                toBridge bytes_bridge $ BS.pack packet


        ch <- newEmptyMVar

        forkIO $ forever $ do
                bs <- fromBridge bytes_bridge
                sequence_ [ putMVar ch b
                          | b <- BS.unpack bs
                          ]

        let nextByte = takeMVar ch

        let recvFrame bs = do
                b <- nextByte
                if b == end then if null bs then recvFrame []
                                            else return $ BS.pack $ reverse bs
                            else recvFrame' b bs

            recvFrame' b bs | b == esc = do
                    b2 <- nextByte
                    case () of
                      _ | b2 == esc_end -> recvFrame (end:bs)
                        | b2 == esc_esc -> recvFrame (esc:bs)
                        | otherwise     -> recvFrame (b2:bs)     -- just pass on b2, per protocol (violation)
            recvFrame' b bs = recvFrame (b:bs)

        return $ Bridge { toBridge = sendFrame, fromBridge = recvFrame [] }

{-
-- We tag the End-of-Frame
data FrameEnd a = FrameEnd a Bool
 --  FramedAtEnd

slipTX :: Bus U8 -> Fabric (Bus (FrameEnd U8))
slipRX :: Bus (FrameEnd U8) -> Fabric (Bus U8)

udpTX :: Bus (FrameEnd U8) -> F (Bus (FramedEnd U8))

-}

