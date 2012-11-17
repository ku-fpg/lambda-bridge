{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, DataKinds, KindSignatures #-}

module Network.LambdaBridge.Serial where

import System.Hardware.Serialport
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.Maybe

import Network.LambdaBridge.Bridge


-- On linux, we need to give permissions to read/write the TTY,
-- and
--   $ chmod a+rw /dev/ttyS0
--   $ stty -F /dev/ttyS0
--   $ stty -F /dev/ttyS0 raw -echo -parity -istrip -parenb cstopb 115200

lbSerialDefault :: SerialPortSettings
lbSerialDefault = SerialPortSettings { commSpeed   = CS115200,
                                    bitsPerWord = 8,
                                    stopb       = One,
                                    parity      = NoParity,
                                    flowControl = NoFlowControl,
                                    timeout     = 10
				  }

serialDriver :: FilePath	        -- ^ The filename of the serial port, such as /dev/ttyS0 or /dev/ttyUSB0
             -> SerialPortSettings	-- ^ The settings; has default
             -> IO (Bridge Streamed Unchecked)
serialDriver filePath settings = do
        port <- openSerial filePath settings
        setDTR port False
        setRTS port False

        let writeByteString :: ByteString -> IO ()
            writeByteString bs = do
                    n <- send port $ B.pack $ fmap (chr . fromIntegral) $ BS.unpack bs
                    if n < len then writeByteString (BS.drop n bs)
                               else return ()
                where len = BS.length bs

            readByteString :: IO ByteString
            readByteString = do
                   bs <- recv port 16
                   if B.length bs == 0
                      then readByteString
                      else return $ BS.pack $ fmap (fromIntegral . ord) $ B.unpack bs

        return $ Bridge
               { toBridge = writeByteString
               , fromBridge = readByteString
               }


main = do
        br <- serialDriver "/dev/master" lbSerialDefault
        echo (BS.map (\ c -> if c >= 32 then c+1 else c)) br
        print ()
