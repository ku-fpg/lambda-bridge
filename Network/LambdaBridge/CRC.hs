{-# LANGUAGE DataKinds #-}

module Network.LambdaBridge.CRC (crc16Protocol) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Digest.CRC16

import Network.LambdaBridge.Logging (debugM)
import Network.LambdaBridge.Bridge

import Data.Word
import Data.Bits

debug = debugM "lambda-bridge.crc"


{- | Provide Packet Checking by appending a 16-bit CRCs.
     We do not support zero length packets.

From <http://en.wikipedia.org/wiki/Computation_of_CRC>

If the data is destined for serial communication, it is best to use the bit ordering the data
will ultimately be sent in. This is because a CRC's ability to detect burst errors is based on
proximity in the message polynomial M(x); if adjacent polynomial terms are not transmitted
sequentially, a physical error burst of one length may be seen as a longer burst due to the
rearrangement of bits.

For example, both IEEE 802 (ethernet) and RS-232 (serial port) standards specify
least-significant bit first (little-endian) transmission, so a software CRC implementation to
protect data sent across such a link should map the least significant bits in each byte to
coefficients of the highest powers of x. On the other hand, floppy disks and most hard drives
write the most significant bit of each byte first.

We can test things with: <http://www.lammertbies.nl/comm/info/crc-calculation.html>

We use CRC-CCITT (0xFFFF), and the following Frame format:

> <- sync+size -><- HDR CRC  -><- payload + CRC             ->
> +------+------+------+------+----------------+------+------+
> | 0x0  |  sz  |   CRC-16    |  ... DATA .... |   CRC-16    |
> +------+------+------+------+----------------+------+------+

-}

crc16Protocol :: Bridge Framed integrity -> IO (Bridge Framed Checked)
crc16Protocol bridge = do
        let loop = do
                bs <- fromBridge bridge
                case unframeCRC bs of
                  Nothing -> do
                        debug $ "CRC frame failed"
                        loop
                  Just bs' -> do
                        debug $ "CRC frame success"
                        return bs'

        return $ Bridge
              { toBridge = toBridge bridge . frameCRC
              , fromBridge = loop
              }


frameCRC :: ByteString -> ByteString
frameCRC bs =
        -- Adding a CRC'd length helps avoid false +ve CRCs
        addCRC (BS.pack [0, fromIntegral (BS.length bs)]) `BS.append`
        addCRC bs


addCRC :: ByteString -> ByteString
addCRC bs = bs `BS.append`
            BS.pack [ reverseBits $ fromIntegral $ crc_val `div` 256
		    , reverseBits $ fromIntegral $ crc_val `mod` 256
		    ]
     where
        crc_val = crc $ BS.unpack bs


unframeCRC :: ByteString -> Maybe ByteString
unframeCRC bs | len > 6   = do hdr <- checkCRC (BS.take 4 bs)
                               if fromIntegral (BS.index hdr 1) == len - 6
                                       then checkCRC (BS.drop 4 bs)
                                       else Nothing
              | otherwise = Nothing
  where len = BS.length bs

-- We do not allow empty datums, so need to be *greater* than 2 (the checksum)
checkCRC :: ByteString -> Maybe ByteString
checkCRC bs | len > 2 && crc (BS.unpack bs) == 0
                        = Just $ BS.take (len - 2) bs
            | otherwise = Nothing
  where len = BS.length bs

crc :: [Word8] -> Word16
crc = foldl (crc16Update 0x1021 True) 0xffff

reverseBits :: Word8 -> Word8
reverseBits x = sum [ 2^(7-i)
                    | i <- [0..7]
                    , x `testBit` i
                    ]
