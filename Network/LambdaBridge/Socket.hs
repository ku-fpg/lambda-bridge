{-# LANGUAGE DataKinds #-}

module Network.LambdaBridge.Socket where
import qualified Data.ByteString as BS

-- | 'Socket' is a lambda-bridge specific socket; a two-directional pipe that Tx/Rx's packets.

data Socket = Socket
        { send :: BS.ByteString -> IO ()
        , recv :: IO BS.ByteString
        }

