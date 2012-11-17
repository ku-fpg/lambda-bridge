{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, DataKinds, KindSignatures #-}

-- | This module provide the ability to build a Bridge from MVars.

module Network.LambdaBridge.MVar (mVarBridge) where

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Logging

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Concurrent.MVar

debug = debugM "lambda-bridge.mvar"

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

--
